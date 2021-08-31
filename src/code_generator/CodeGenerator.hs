module CodeGenerator where

import Data.List (intercalate)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import CodeGenerationCommonDeclarations
import CommonDeclarations

import StatementGenerator (generateStatement)

preamble :: [String]
preamble = [
    "target triple = \"x86_64-pc-linux-gnu\"",
    "",
    "%_Array = type { i32, i8* }",
    "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare i8* @" ++ stringConcatenationFunction ++ "(i8*, i8*)",
    "declare void @_initial_bookkeeping()",
    "declare void @_final_bookkeeping()",
    "declare %_Array* @" ++ arrayAllocationFunction ++ "(i32, i32)",
    "declare i8* @" ++ objectAllocationFunction ++ "(i32)",
    "declare i32 @" ++ arrayLengthFunction ++ "(%_Array*)",
    "declare void @" ++ initializeStringArrayFunction ++ "(%_Array*, i8*)",
    "",
    "define i32 @main() {",
    "  call void @_initial_bookkeeping()",
    "  %main_function_result = call i32 @_main()",
    "  call void @_final_bookkeeping()",
    "  ret i32 %main_function_result",
    "}",
    ""]

runGenerateCode :: (CProgram, StringConstants) -> String -> IO ()
runGenerateCode (prog@(CProgram _ _ classes), strConsts) outputFile = do
  outputFileExists <- doesFileExist outputFile
  when outputFileExists (removeFile outputFile)
  let
    globalLabel = Label (show (0 :: Integer))
    codeGenerationState = CodeGenerationState {
      fileName = outputFile,
      freeRegister = 0,
      freeLabel = 0,
      currentLabel = globalLabel,
      returnLabel = globalLabel,
      returnRegister = Register "_return_value",
      stringConstants = strConsts,
      classHeaders = classes
    }
  _ <- evalStateT (runExceptT (runReaderT (generateCode prog) ())) codeGenerationState
  return ()

generateConstantString :: (String, (String, Integer)) -> CodeGenerationMonad
generateConstantString (value, (name, len)) = append $ unwords [
  "@" ++ name, "=",
  "private unnamed_addr constant [" ++ show len ++ " x i8] c\"" ++ value ++ "\""]

generateCode :: CProgram -> CodeGenerationMonad
generateCode (CProgram classDefinitions functionDefinitions classes) = do
  strConsts <- gets stringConstants
  -- mapM_ (liftIO . print) classDefinitions
  -- mapM_ (liftIO . print) functionDefinitions
  append $ intercalate "\n" preamble
  mapM_ generateConstantString (Map.toList strConsts)
  mapM_ generateClassStruct (Map.toList classes)
  mapM_ generateClassMethods classDefinitions
  mapM_ generateFunctionCode functionDefinitions

generateClassStruct :: (ClassName, CClassHeader) -> CodeGenerationMonad
generateClassStruct (className, CClassHeader _ types _) = do
  let
    llvmTypes = map getLLVMType types
    structType = "<{" ++ intercalate ", " llvmTypes ++ "}>"
  append (getLLVMClassName className ++ " = type " ++ structType ++ "\n")

generateClassMethods :: CClsDef -> CodeGenerationMonad
generateClassMethods (CClsDef className _ methodDefs) = mapM_ (generateMethodCode className) methodDefs

generateMethodCode :: CIdent -> CMetDef -> CodeGenerationMonad
generateMethodCode className (CMetDef t name args (CBlock body)) = generateFunctionOrMethod True t ("$" ++ className ++ "$" ++ name) (CArg (CClass className) "$this" 0 : args) body

generateFunctionCode :: CFnDef -> CodeGenerationMonad
generateFunctionCode (CFnDef t name args (CBlock body)) = generateFunctionOrMethod False t name args body

generateFunctionOrMethod :: Bool -> CType -> CIdent -> [CArg] -> [CStatement] -> CodeGenerationMonad
generateFunctionOrMethod isMethod t name args body = do
  let retLabel = Label "ReturnLabel"
  retRegister <- gets returnRegister
  modify (\e -> e { freeRegister = 0, freeLabel = 0, currentLabel = Label (show (0 :: Integer)), returnLabel = retLabel })
  argValueRegisters <- replicateM (length args) getFreeRegister
  let
    argsAndRegisters = zip args argValueRegisters
    registerArgs = map (\(CArg cType _ _, reg) -> getLLVMType cType ++ " " ++ show reg) argsAndRegisters
    argsTxt = intercalate ", " registerArgs
    storeRegisterValueInArgument :: (CArg, Value) -> CodeGenerationMonad
    storeRegisterValueInArgument (CArg cType ident loc, reg) = do
      varRegister <- getVarRegister (CLocalVariable ident loc)
      emitAlloca varRegister cType
      emitStore cType reg varRegister
  append $ unwords [
    "define",
    getLLVMType t,
    -- main translated to _main, because some bookkeeping (ex. memory management)
    -- code needs to be added at the beginning and end of the program
    "@" ++ if name == "main" && not isMethod then "_main" else name,
    "(" ++ argsTxt ++ ") {"]
  when (t /= CVoid) (emitAlloca retRegister t)
  mapM_ storeRegisterValueInArgument argsAndRegisters
  mapM_ generateStatement body
  emitBranchUnconditional retLabel
  emitLabel retLabel
  if t == CVoid
    then
      appendIndent $ unwords ["ret", getLLVMType CVoid]
    else do
      retTempRegister <- getFreeRegister
      emitLoad retTempRegister t retRegister
      appendIndent $ unwords ["ret", getLLVMType t, show retTempRegister]
  append $ unwords ["}"]
  newLine
