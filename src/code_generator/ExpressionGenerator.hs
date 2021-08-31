module ExpressionGenerator where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import CodeGenerationCommonDeclarations
import CommonDeclarations

generateExpression :: CExpression -> CodeGenerationMonadT Value

generateExpression (CVar t cVariable) = do
  r <- getFreeRegister
  varRegister <- getVarRegister cVariable
  emitLoad r t varRegister
  return r

generateExpression (CLitInt v) = return $ IntValue v

generateExpression CLitTrue = return $ BoolValue True

generateExpression CLitFalse = return $ BoolValue False

generateExpression (CApp t fn args) = do
  argValues <- mapM generateExpression args
  let
    argTypes = map getExpressionCType args
    preparedArgs = zip argTypes argValues
  if t == CVoid then do
    emitVoidCall fn preparedArgs
    return VoidValue
  else do
    r <- getFreeRegister
    emitCall r t fn preparedArgs
    return r

generateExpression (CMetApp actualClassName calledClassName cType metName args) = do
  argValues <- mapM generateExpression args
  let
    llvmMetName = "$" ++ calledClassName ++ "$" ++ metName
    argTypes = map getExpressionCType args
  preparedArgs <- if actualClassName == calledClassName
    then do
      selfRef <- generateExpression (CVar (CClass actualClassName) CSelf)
      return ((CClass actualClassName, selfRef) : zip argTypes argValues)
    else do
      selfRef <- generateExpression (CVar (CClass actualClassName) CSelf)
      castPtr <- getFreeRegister
      emitBitcast castPtr (getLLVMType (CClass actualClassName)) (show selfRef) (getLLVMType (CClass calledClassName))
      return ((CClass calledClassName, castPtr) : zip argTypes argValues)
  if cType == CVoid then do
    emitVoidCall llvmMetName preparedArgs
    return VoidValue
  else do
    r <- getFreeRegister
    emitCall r cType llvmMetName preparedArgs
    return r

generateExpression (CString str) = do
  consts <- gets stringConstants
  reg <- getFreeRegister
  case Map.lookup str consts of
    Just (constName, len) -> do
      emitBitcast reg ("[" ++ show len ++ " x i8]*") ("@" ++ constName) "i8*"
      return reg
    Nothing -> error ("string constant " ++ show str ++ " not found")

generateExpression (CLitNull _) = return $ PtrValue "null"

generateExpression (CLitCls className) = do
  allocatedVoidPtrObject <- getFreeRegister
  (CClassHeader objectSize _ attrs) <- getClassHeader className
  emitVoidPtrCall allocatedVoidPtrObject objectAllocationFunction [(CInt, IntValue objectSize)]
  objectPtr <- getFreeRegister
  emitBitcast objectPtr "i8*" (show allocatedVoidPtrObject) (getLLVMClassName className ++ "*")
  let
    assignDefaultValue :: (VariableName, (CType, Integer)) -> CodeGenerationMonad
    assignDefaultValue (attrName, (cType, _)) = do
      defaultValue <- generateExpression (getDefaultCExpression cType)
      (Just (fieldPointer, _), fieldType) <- dereferenceNestedClassMember objectPtr [CDMemVar className attrName]
      emitStore fieldType defaultValue fieldPointer
    maybeAssignDefaultValue :: (VariableName, (CType, Integer)) -> CodeGenerationMonad
    maybeAssignDefaultValue attr@(_, (CStr, _)) = assignDefaultValue attr
    maybeAssignDefaultValue attr@(_, (CArray{}, _)) = assignDefaultValue attr
    maybeAssignDefaultValue _ = return ()
  mapM_ maybeAssignDefaultValue (Map.toList attrs)
  return objectPtr

generateExpression (CCast from expression to) = do
  result <- generateExpression expression
  castResult <- getFreeRegister
  emitBitcast castResult (getLLVMType from) (show result) (getLLVMType to)
  return castResult

generateExpression (CExprAttr _ expression attrs) = do
  objectRegister <- generateExpression expression
  (fieldPointer, fieldType) <- dereferenceNestedClassMember objectRegister attrs
  case fieldPointer of
    Just (fp, dereference) ->
      if dereference
        then do
          fieldValue <- getFreeRegister
          emitLoad fieldValue fieldType fp
          return fieldValue
        else return fp
    Nothing -> return VoidValue

generateExpression expr@(CLitArr t e) = do
  arraySize <- generateExpression e
  allocatedArray <- getFreeRegister
  emitCall allocatedArray (getExpressionCType expr) arrayAllocationFunction [(CInt, arraySize), (CInt, IntValue (getCTypeSize t))]
  case t of
      CStr -> do
        emptyString <- generateExpression (CString "\\00")
        emitVoidCall initializeStringArrayFunction [(CArray CStr, allocatedArray), (CStr, emptyString)]
      _ -> return ()
  return allocatedArray

generateExpression (CArrayLength expr) = do
  array <- generateExpression expr
  lengthFieldPointer <- getFreeRegister
  emitGetElementPointer lengthFieldPointer "%_Array" array [IntValue 0, IntValue 0]
  arrayLength <- getFreeRegister
  emitLoad arrayLength CInt lengthFieldPointer
  return arrayLength

generateExpression (CNeg e) = do
  v <- generateExpression e
  reg <- getFreeRegister
  appendIndent $ unwords [show reg, "=", "sub", getLLVMType CInt, "0,", show v]
  return reg

generateExpression (CNot e) = do
  v <- generateExpression e
  reg <- getFreeRegister
  appendIndent $ unwords [show reg, "=", "xor", getLLVMType CBool, "1,", show v]
  return reg

generateExpression (CMulInt e1 op e2) =
  intBinaryOp opStr e1 e2
  where
    opStr = case op of
      CTimes -> "mul"
      CDiv -> "sdiv"
      CMod -> "srem"

generateExpression (CAddString e1 e2) = generateExpression (CApp CStr stringConcatenationFunction [e1, e2])

generateExpression (CAddInt e1 op e2) = do
  let
    opStr = case op of
      CPlus -> "add"
      CMinus -> "sub"
  intBinaryOp opStr e1 e2

generateExpression (CRel expr1 relOp expr2) = do
  reg <- getFreeRegister
  reg1 <- generateExpression expr1
  reg2 <- generateExpression expr2
  let
    exprType = getExpressionCType expr1
  emitCompare reg relOp exprType reg1 reg2
  return reg

generateExpression (CAnd expr1 expr2) = boolBinaryOp AndOperation expr1 expr2

generateExpression (COr expr1 expr2) = boolBinaryOp OrOperation expr1 expr2

data BinaryOperation = OrOperation | AndOperation
  deriving Show
boolBinaryOp :: BinaryOperation -> CExpression -> CExpression -> CodeGenerationMonadT Value
boolBinaryOp op expr1 expr2 = do
  res1 <- generateExpression expr1
  initialLabel <- gets currentLabel
  [labelSecondExpression, labelAfter] <- replicateM 2 getFreeLabel
  case op of
    AndOperation -> emitBranchIfElse res1 labelSecondExpression labelAfter
    OrOperation -> emitBranchIfElse res1 labelAfter labelSecondExpression
  emitLabel labelSecondExpression
  res2 <- generateExpression expr2
  emitBranchUnconditional labelAfter
  outgoingLabel <- gets currentLabel
  emitLabel labelAfter
  reg <- getFreeRegister
  case op of
    AndOperation -> emitPhi reg [("false", initialLabel), (show res2, outgoingLabel)]
    OrOperation -> emitPhi reg [("true", initialLabel), (show res2, outgoingLabel)]
  return reg

intBinaryOp :: String -> CExpression -> CExpression -> CodeGenerationMonadT Value
intBinaryOp opStr e1 e2 = do
  v1 <- generateExpression e1
  v2 <- generateExpression e2
  r <- getFreeRegister
  let typeString = getLLVMType CInt
  appendIndent $ unwords [show r, "=", opStr, typeString, show v1 ++ ",", show v2]
  return r

getArrayItemPointer :: CType -> Value -> CExpression -> CodeGenerationMonadT Value
getArrayItemPointer itemType arrayRegister indexExpression = do
  indexExpressionValue <- generateExpression indexExpression
  arrayVoidPointer <- getFreeRegister
  emitGetElementPointer arrayVoidPointer "%_Array" arrayRegister [IntValue 0, IntValue 1]
  arrayPointer <- getFreeRegister
  let typeStr = getLLVMType itemType
  emitBitcast arrayPointer "i8**" (show arrayVoidPointer) (typeStr ++ "**")
  itemPointerWithoutOffset <- getFreeRegister
  emitLoadPtr itemPointerWithoutOffset itemType arrayPointer
  itemPointerWithOffset <- getFreeRegister
  emitGetElementPointer itemPointerWithOffset typeStr itemPointerWithoutOffset [indexExpressionValue]
  return itemPointerWithOffset

dereferenceNestedClassMember :: Value -> [CDMem] -> CodeGenerationMonadT (Maybe (Value, Bool), CType)
dereferenceNestedClassMember _ [] = error "empty attr list"
dereferenceNestedClassMember objectPointer [attr] = dereferenceClassMember objectPointer attr

dereferenceNestedClassMember objectPointer (attr : attrs@(_:_)) = do
  (Just (fieldPointer, dereference), fieldType) <- dereferenceClassMember objectPointer attr
  fieldValue <- if dereference
    then do
      fv <- getFreeRegister
      emitLoad fv fieldType fieldPointer
      return fv
    else return fieldPointer
  dereferenceNestedClassMember fieldValue attrs

dereferenceClassMember :: Value -> CDMem -> CodeGenerationMonadT (Maybe (Value, Bool), CType)
dereferenceClassMember objectPointer (CDMemVar className attr) = do
  fieldPointer <- getFreeRegister
  (CClassHeader _ _ attrs) <- getClassHeader className
  let
    classLLVMName = getLLVMClassName className
    (fieldType, fieldOffset) = case Map.lookup attr attrs of
      Just (ft, fo) -> (ft, fo)
      Nothing -> error "field offset not found for attr"
  emitGetElementPointer fieldPointer classLLVMName objectPointer [IntValue 0, IntValue fieldOffset]
  return (Just (fieldPointer, True), fieldType)

dereferenceClassMember objectPointer (CDMemMet calledClassName definitionClassName cType metName args) = do
  argValues <- mapM generateExpression args
  let
    llvmMetName = "$" ++ definitionClassName ++ "$" ++ metName
    argTypes = map getExpressionCType args
  preparedArgs <- if definitionClassName == calledClassName
    then
      return ((CClass calledClassName, objectPointer) : zip argTypes argValues)
    else do
      castPtr <- getFreeRegister
      emitBitcast castPtr (getLLVMType (CClass calledClassName)) (show objectPointer) (getLLVMType (CClass definitionClassName))
      return ((CClass definitionClassName, castPtr) : zip argTypes argValues)
  if cType == CVoid then do
    emitVoidCall llvmMetName preparedArgs
    return (Nothing, cType)
  else do
    r <- getFreeRegister
    emitCall r cType llvmMetName preparedArgs
    return (Just (r, False), cType)

dereferenceClassMember arrayPointer (CDMemInd itemType indexExpression) = do
  itemPointerWithOffset <- getArrayItemPointer itemType arrayPointer indexExpression
  return (Just (itemPointerWithOffset, True), itemType)

dereferenceClassMember arrayPointer CDLength = do
  lengthFieldPointer <- getFreeRegister
  emitGetElementPointer lengthFieldPointer "%_Array" arrayPointer [IntValue 0, IntValue 0]
  lengthValue <- getFreeRegister
  emitLoad lengthValue CInt lengthFieldPointer
  return (Just (lengthValue, False), CInt)
