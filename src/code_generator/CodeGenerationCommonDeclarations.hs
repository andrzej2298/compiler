module CodeGenerationCommonDeclarations where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import CommonDeclarations

type CodeGenerationMonadT a = ReaderT () (ExceptT Error
                                         (StateT CodeGenerationState IO)) a
type CodeGenerationMonad = CodeGenerationMonadT ()
data CodeGenerationState = CodeGenerationState {
  fileName :: String,
  freeRegister :: Integer,
  freeLabel :: Integer,
  currentLabel :: Label,
  returnLabel :: Label,
  returnRegister :: Value,
  stringConstants :: StringConstants,
  classHeaders :: CClassHeaders
}

newtype Label = Label String
instance Show Label where
  show (Label s) = s

stringConcatenationFunction :: String
stringConcatenationFunction = "_concatenate_strings"
arrayAllocationFunction :: String
arrayAllocationFunction = "_allocate_flat_array"
objectAllocationFunction :: String
objectAllocationFunction = "_allocate_object"
arrayLengthFunction :: String
arrayLengthFunction  = "_array_length"
initializeStringArrayFunction :: String
initializeStringArrayFunction = "_initialize_string_array"

append :: String -> CodeGenerationMonad
append str = do
  outputFile <- gets fileName
  liftIO $ appendFile outputFile str
  liftIO $ appendFile outputFile "\n"

newLine :: CodeGenerationMonad
newLine = append ""

appendIndent :: String -> CodeGenerationMonad
appendIndent str = append ("  " ++ str)

appendIndentList :: [String] -> CodeGenerationMonad
appendIndentList = mapM_ appendIndent

appendIndentDebug :: String -> CodeGenerationMonad
appendIndentDebug str = append ("  " ++ str)

getFreeRegister :: CodeGenerationMonadT Value
getFreeRegister = do
  r <- gets freeRegister
  modify (\e -> e { freeRegister = r + 1 })
  return $ Register ("_r" ++ show r)

getFreeLabel :: CodeGenerationMonadT Label
getFreeLabel = do
  l <- gets freeLabel
  modify (\e -> e { freeLabel = l + 1 })
  return $ Label ("L" ++ show l)

getVarRegister :: CVariable -> CodeGenerationMonadT Value
getVarRegister (CLocalVariable ident loc) = return (Register (ident ++ "$" ++ show loc))
getVarRegister (CAttributeVariable className x) = do
  fieldPointer <- getFreeRegister
  r <- getFreeRegister
  (CClassHeader _ _ attrs) <- getClassHeader className
  let
    classLLVMName = getLLVMClassName className
    (_, fieldOffset) = case Map.lookup x attrs of
      Just (ft, fo) -> (ft, fo)
      Nothing -> error "field offset for self not found for attr"
  emitLoad r (CClass className) (Register "$this$0")
  emitGetElementPointer fieldPointer classLLVMName r [IntValue 0, IntValue fieldOffset]
  return fieldPointer
getVarRegister CSelf = return (Register "$this$0")


getClassHeader :: ClassName -> CodeGenerationMonadT CClassHeader
getClassHeader className = do
  classes <- gets classHeaders
  let
    classHeader = fromMaybe (error "object size not found") (Map.lookup className classes)
  return classHeader

-------------------------------- LLVM IR
emitBranchIfElse :: Value -> Label -> Label -> CodeGenerationMonad
emitBranchIfElse conditionalValue labelThen labelElse =
  appendIndent $ unwords ["br", getLLVMType CBool, show conditionalValue, ",", "label", "%" ++ show labelThen, ",", "label", "%" ++ show labelElse]

emitBranchUnconditional :: Label -> CodeGenerationMonadT ()
emitBranchUnconditional jumpLabel =
  appendIndent $ unwords ["br", "label", "%" ++ show jumpLabel]

emitPhi :: Value -> [(String, Label)] -> CodeGenerationMonad
emitPhi reg pairs =
  appendIndent
    (unwords [show reg, "=", "phi", getLLVMType CBool]
     ++ " "
     ++ intercalate ", " (map (\(value, jumpLabel) -> unwords ["[", value, ",", "%" ++ show jumpLabel, "]"]) pairs))

emitStore :: CType -> Value -> Value -> CodeGenerationMonad
emitStore type_ source dest =
  appendIndent $ unwords ["store", typeStr, show source ++ ",", typeStr ++ "*", show dest]
  where typeStr = getLLVMType type_

emitAlloca :: Value -> CType  -> CodeGenerationMonad
emitAlloca register type_  =
  appendIndent $ unwords [show register, "=", "alloca", typeStr]
  where typeStr = getLLVMType type_

emitLoad :: Value -> CType -> Value -> CodeGenerationMonad
emitLoad register type_ source =
  appendIndent $ unwords [show register, "=", "load", typeStr ++ ",", typeStr ++ "*", show source]
  where typeStr = getLLVMType type_

emitLoadPtr :: Value -> CType -> Value -> CodeGenerationMonad
emitLoadPtr register type_ source =
  appendIndent $ unwords [show register, "=", "load", typeStr ++ "*,", typeStr ++ "**", show source]
  where typeStr = getLLVMType type_

emitCall :: Value -> CType -> String -> [(CType, Value)] -> CodeGenerationMonad
emitCall r t fn args =
  let
    argsText = intercalate ", " $ map (\(cType, value) -> getLLVMType cType ++ " " ++ show value) args
  in
    appendIndent $ unwords [show r, "=", "call", getLLVMType t, "@" ++ fn ++ "(" ++ argsText ++ ")"]

emitVoidCall :: String -> [(CType, Value)] -> CodeGenerationMonad
emitVoidCall fn args =
  let
    argsText = intercalate ", " $ map (\(cType, value) -> getLLVMType cType ++ " " ++ show value) args
  in
    appendIndent $ unwords ["call", getLLVMType CVoid, "@" ++ fn ++ "(" ++ argsText ++ ")"]

emitVoidPtrCall :: Value -> String -> [(CType, Value)] -> CodeGenerationMonad
emitVoidPtrCall r fn args =
  let
    argsText = intercalate ", " $ map (\(cType, value) -> getLLVMType cType ++ " " ++ show value) args
  in
    appendIndent $ unwords [show r, "=", "call", "i8*", "@" ++ fn ++ "(" ++ argsText ++ ")"]

emitLabel :: Label -> CodeGenerationMonad
emitLabel l = do
  modify (\e -> e { currentLabel = l })
  append (show l ++ ":")

emitGetElementPointer :: Value -> String -> Value -> [Value] -> CodeGenerationMonad
emitGetElementPointer result typeStr inputPointer indices =
  appendIndent $ unwords [show result, "=", "getelementptr", typeStr, ",", typeStr ++ "*", show inputPointer,
    unwords $ map (\i -> ", " ++ intTypeStr ++ " " ++ show i) indices]
  where
    intTypeStr = getLLVMType CInt

emitBitcast :: Value -> String -> String -> String -> CodeGenerationMonad
emitBitcast reg from value to =
  appendIndent $ unwords [show reg, "=", "bitcast", from, value, "to", to]

emitCompare :: Value -> CRelOp -> CType -> Value -> Value -> CodeGenerationMonad
emitCompare reg relOp exprType reg1 reg2 = do
  let
    relOpTxt = case relOp of
      CLTH -> "slt"
      CLE -> "sle"
      CGTH -> "sgt"
      CGE -> "sge"
      CEQU -> "eq"
      CNE -> "ne"
  appendIndent $ unwords [show reg, "=", "icmp", relOpTxt, getLLVMType exprType, show reg1 ++ ",", show reg2]
