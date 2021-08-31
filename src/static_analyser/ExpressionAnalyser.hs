module ExpressionAnalyser where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import StaticAnalysisCommonDeclarations
import CommonDeclarations
import AbsLatte

analyseExpression :: Expression -> StaticAnalysisMonadT (TypeValue, CExpression)

analyseExpression (EVar c (Ident varName)) = do
   (t, cVar) <- getVarTypeNoAss varName c
   return (t, CVar (getCType t) cVar)

analyseExpression (ELitInt c v)
  | v <= maxInt = return (Int c, CLitInt v)
  | otherwise = throwError $ StaticError "integer doesn't fit in 32 bits" c
  where
    maxInt = 2^maxPower - 1

analyseExpression (ELitTrue c) = return (Bool c, CLitTrue)

analyseExpression (ELitFalse c) = return (Bool c, CLitFalse)

analyseExpression (EApp c (Ident fn) actualArgs) = do
  scopeClass <- asks classThis
  (Fun _ resultType formalArgs, className) <- functionLookup fn c
  cExpressions <- analyseApplication formalArgs actualArgs c
  let
    cExpression = case (className, scopeClass) of
      (Just calledClassName, Just actualClassName) -> CMetApp actualClassName calledClassName (getCType resultType) fn cExpressions
      (Just _, Nothing) -> error "method must have and actualClassName"
      (Nothing, _) -> CApp (getCType resultType) fn cExpressions
  return (resultType, cExpression)

analyseExpression (EString c str) = do
  let
    strTruncated = (drop 1 . take (length str - 1)) str
  (escaped, escapedLength) <- escapeString strTruncated c
  let nullTerminated = escaped ++ "\\00"
  strConsts <- gets stringConstants
  case Map.lookup nullTerminated strConsts of
    Just _ -> return ()
    Nothing -> insertStringConstant nullTerminated (escapedLength + 1)
  return (Str c, CString nullTerminated)

analyseExpression (ELitNull c (EVar _ (Ident className))) = do
  _ <- getClassHeader className c
  return (Class dummyCursor (Ident className), CLitNull className)

analyseExpression (ELitNull c _) = throwError $ TypeError "cast type invalid" c

analyseExpression (ELitArr c t e) = do
  let
    cType = getCType t
  checkUsedClassExists c cType
  (lengthExpressionType, lengthCExpression) <- analyseExpression e
  when (lengthExpressionType /= dummyInt) (throwError $ TypeError "array length must be of type int" c)
  case cType of
    CArray{} -> throwError $ TypeError "only one-dimensional arrays allowed" c
    CVoid -> throwError $ TypeError "arrays of type void not allowed" c
    _ -> return (Array c t, CLitArr cType lengthCExpression)

analyseExpression (ELitCls c classType@(Class _ (Ident className))) = do
  _ <- getClassHeader className c
  return (classType, CLitCls className)

analyseExpression (ELitCls c _) = throwError $ TypeError "new must be followed by a class name or array with length" c

analyseExpression (ENameAttr c varName attrs) = analyseExpression (EExprAttr c (EVar c varName) attrs)

analyseExpression (EExprAttr _ e attrs) = do
  (expressionType, cExpression) <- analyseExpression e
  (dereferencedValueType, dereferenceList) <- dereferenceAttributes expressionType attrs False
  return (dereferencedValueType, CExprAttr (getCType dereferencedValueType) cExpression dereferenceList)

analyseExpression (Neg c (ELitInt _ v))
  | -v >= minInt = return (Int c, CLitInt (-v))
  | otherwise = throwError $ StaticError "integer doesn't fit in 32 bits" c
  where
    minInt = -2^maxPower

analyseExpression (Neg c e) = do
  (t, cExpression) <- analyseExpression e
  case t of
    Int{} -> return (Int c, CNeg cExpression)
    v -> throwError $ TypeError ("can't negate value of type " ++ show v) c

analyseExpression (Not c e) = do
  (t, cExpression) <- analyseExpression e
  case t of
    Bool _ -> return (Bool c, CNot cExpression)
    v -> throwError $ TypeError ("can't negate value of type " ++ show v) c

analyseExpression (EMul c e1 op e2) = do
  (t1, cExpr1) <- analyseExpression e1
  (t2, cExpr2) <- analyseExpression e2
  case (t1, t2) of
    (Int _, Int _) -> return (Int c, CMulInt cExpr1 (getCMulOp op) cExpr2)
    (v1, v2) -> throwError $
      TypeError ("can't multiply or divide " ++ show v1 ++ " and " ++ show v2) c

analyseExpression (EAdd c e1 op e2) = do
  (t1, cExpr1) <- analyseExpression e1
  (t2, cExpr2) <- analyseExpression e2
  case (t1, op, t2) of
    (Int _, _, Int _) -> return (Int c, CAddInt cExpr1 (getCAddOp op) cExpr2)
    (Str _, Plus _, Str _) -> return (Str c, CAddString cExpr1 cExpr2)
    (v1, _, v2) -> throwError $
      TypeError ("can't add or subtract " ++ show v1 ++ " and " ++ show v2) c

analyseExpression (ERel c e1 op e2) = do
  (t1, cExpr1) <- analyseExpression e1
  (t2, cExpr2) <- analyseExpression e2
  let
    err t1' t2' = TypeError ("can't compare " ++ show t1' ++ " and " ++ show t2') c
    checkEqual :: TypeValue -> TypeValue -> StaticAnalysisMonadT (TypeValue, CExpression)
    checkEqual t u
      | t == u = return (Bool c, CRel cExpr1 (getRelOp op) cExpr2)
      | otherwise = throwError $ err t1 t2
  case (t1, op, t2) of
    (Int _, _, Int _) -> return (Bool c, CRel cExpr1 (getRelOp op) cExpr2)
    (Void _, _, _) -> throwError $ err t1 t2
    (_, _, Void _) -> throwError $ err t1 t2
    (_, EQU _, _) -> checkEqual t1 t2
    (_, NE _, _) -> checkEqual t1 t2
    _ -> throwError $ err t1 t2

analyseExpression (EAnd c e1 e2) = do
  (t, ce1, ce2) <- analyseLogicExpression c e1 e2
  return (t, CAnd ce1 ce2)

analyseExpression (EOr c e1 e2) = do
  (t, ce1, ce2) <- analyseLogicExpression c e1 e2
  return (t, COr ce1 ce2)

analyseLogicExpression :: Cursor -> Expression -> Expression -> StaticAnalysisMonadT (TypeValue, CExpression, CExpression)
analyseLogicExpression c e1 e2 = do
  (t1, cExpr1) <- analyseExpression e1
  (t2, cExpr2) <- analyseExpression e2
  case (t1, t2) of
    (Bool _, Bool _) -> return (Bool c, cExpr1, cExpr2)
    _ -> throwError $ TypeError ("can't perform && or || of " ++
                                 show t1 ++ " and " ++ show t2) c

analyseApplication :: [TypeValue] -> [Expression] -> Cursor -> StaticAnalysisMonadT [CExpression]
analyseApplication formalArgs actualArgs c = do
  when
    (length formalArgs /= length actualArgs)
    (throwError $ TypeError
     ("wrong number of arguments: expected " ++
      show (length formalArgs) ++ ", got " ++ show (length actualArgs)) c)
  let
    checkArgumentType :: (TypeValue, Expression) -> StaticAnalysisMonadT CExpression
    checkArgumentType (formal, expr) = do
      (actual, cExpression) <- analyseExpression expr
      validSubtype <- isSubtypeOf actual formal
      when (actual /= formal && not validSubtype) (throwError $ typeMismatchError formal actual (getExprCursor expr))
      return (if actual == formal then cExpression else CCast (getCType actual) cExpression (getCType formal))
  mapM checkArgumentType (zip formalArgs actualArgs)

dereferenceAttributes :: TypeValue -> [Member] -> Bool -> StaticAnalysisMonadT (TypeValue, [CDMem])
dereferenceAttributes resultType [] _ = return (resultType, [])

dereferenceAttributes (Class _ (Ident className)) (MemVar c (Ident attrName) : rest) assign = do
  attrType <- findVariableInHierarchy attrName className c
  (resultType, dereferences) <- dereferenceAttributes attrType rest assign
  return (resultType, CDMemVar className attrName : dereferences)

dereferenceAttributes (Class _ (Ident className)) (MemMet c (Ident metName) actualArgs : rest) assign@False = do
  methodType <- methodLookup (Just className) metName c
  (Fun _ metResult formalArgs, definitionClassName) <- case methodType of
    Just res@(Fun{}, _) -> return res
    Just _ -> methodError metName c
    Nothing -> methodError metName c
  cExpressions <- analyseApplication formalArgs actualArgs c
  (resultType, dereferences) <- dereferenceAttributes metResult rest assign
  return (resultType, CDMemMet className definitionClassName (getCType metResult) metName cExpressions : dereferences)
dereferenceAttributes _ (MemMet c _ _ : _) True = throwError $ TypeError "a method can't be used on the left side of an assignment" c

dereferenceAttributes Array{} [MemVar c (Ident "length")] False = return (Int c, [CDLength])

dereferenceAttributes (Array _ type_) (MemInd c indexExpression : rest) assign = do
  (indexExpressionType, indexCExpression) <- analyseExpression indexExpression
  when (indexExpressionType /= dummyInt) (throwError $ TypeError "array index must be of type int" c)
  (resultType, dereferences) <- dereferenceAttributes type_ rest assign
  return (resultType, CDMemInd (getCType type_) indexCExpression : dereferences)

dereferenceAttributes _ (MemVar c (Ident attrName) : _) _ = attributeError attrName c
dereferenceAttributes _ (MemMet c (Ident metName) _ : _) _ = methodError metName c
dereferenceAttributes _ (MemInd c _ : _) _ = throwError $ TypeError "array expected" c

attributeError :: String -> Cursor -> StaticAnalysisMonadT a
attributeError attrName c = throwError $ NameError ("invalid attribute: " ++ attrName) c

methodError :: String -> Cursor -> StaticAnalysisMonadT a
methodError metName c = throwError $ NameError ("invalid method call: " ++ metName) c

insertStringConstant :: String -> Integer -> StaticAnalysisMonadT ()
insertStringConstant str len = modify (\s -> s { stringConstants = Map.insert str ("_s" ++ show (Map.size (stringConstants s)), len) (stringConstants s)})

getClassHeader :: String -> Cursor -> StaticAnalysisMonadT ClassHeader
getClassHeader className c = do
  classes <- asks classTypes
  case Map.lookup className classes of
    Just classHeader -> return classHeader
    Nothing -> throwError $ NameError ("class " ++ className ++ " wasn't defined") c

getVarTypeAss :: VariableName -> Cursor -> StaticAnalysisMonadT (TypeValue, CVariable)
getVarTypeAss = getVariableType True

getVarTypeNoAss :: VariableName -> Cursor -> StaticAnalysisMonadT (TypeValue, CVariable)
getVarTypeNoAss = getVariableType False

getVariableType :: Bool -> VariableName -> Cursor -> StaticAnalysisMonadT (TypeValue, CVariable)
getVariableType assignment x cur = do
  r <- asks variableTypes
  s <- gets variableTypeStore
  l <- case Map.lookup x r of
    Just l' -> return (Left l')
    Nothing -> do
      memberVariable <- getAttributeType assignment x cur
      return (Right memberVariable)
  case l of
    Left l' -> case Map.lookup l' s of
      Just t -> return (t, CLocalVariable x l')
      Nothing -> error "variable pointing to invalid location"
    Right res -> return res

getAttributeType :: Bool -> VariableName -> Cursor -> StaticAnalysisMonadT (TypeValue, CVariable)
getAttributeType assignment x cur = do
  methodClass <- asks classThis
  className <- maybe (variableNotDeclaredError x cur) return methodClass
  case (x, assignment) of
    -- invalid assignment to self
    ("self", True) -> throwError $ StaticError "can't modify \"self\"" cur
    -- valid retrieval of self
    ("self", False) ->
      return (Class cur (Ident className), CSelf)
    -- valid retrieval of normal attribute
    (_, _) -> do
      attributeType <- findVariableInHierarchy x className cur
      return (attributeType, CAttributeVariable className x)

variableNotDeclaredError :: VariableName -> Cursor -> StaticAnalysisMonadT a
variableNotDeclaredError x cur = throwError $ NameError ("variable " ++ x ++ " wasn't declared") cur

findVariableInHierarchy :: VariableName -> ClassName -> Cursor -> StaticAnalysisMonadT TypeValue
findVariableInHierarchy x className cur = do
  ClassHeader superclass _ attributes _ <- getClassHeader className cur
  case (Map.lookup x attributes, superclass) of
    (Just t, _) -> return t
    (Nothing, Just superclassName) -> findVariableInHierarchy x superclassName cur
    (Nothing, Nothing) -> variableNotDeclaredError x cur

escapeString :: String -> Cursor -> StaticAnalysisMonadT (String, Integer)
escapeString = go False where
  mappings = Map.fromList [('\\', "\\\\"), ('a', "\\07"), ('b', "\\08"), ('f', "\\0C"),
    ('n', "\\0A"), ('r', "\\0D"), ('t', "\\09"), ('v', "\\0B"), ('\'', "\\27"), ('"', "\\22")]
  go :: Bool -> String -> Cursor -> StaticAnalysisMonadT (String, Integer)
  go True (c:rest) cur =
    case Map.lookup c mappings of
      Just substituteString -> do
        (restResult, restLength) <- go False rest cur
        return (substituteString ++ restResult, restLength + 1)
      Nothing -> throwError $ StaticError ("unknown escape sequence: \\" ++ [c]) cur
  go True [] cur = throwError $ StaticError "escape sequence unfinished" cur
  go False ('\\':rest) cur = go True rest cur
  go False (c:rest) cur = do
    (restResult, restLength) <- go False rest cur
    return (c : restResult, restLength + 1)
  go False [] _ = return ([], 0)

functionLookup :: FunctionName -> Cursor -> StaticAnalysisMonadT (TypeValue, Maybe ClassName)
functionLookup fn c = do
  functionHeaders <- asks functionTypes
  scopeClass <- asks classThis
  methodType <- methodLookup scopeClass fn c
  case (methodType, Map.lookup fn functionHeaders) of
    (Just (mt, className), _) -> return (mt, Just className)  -- methods are prioritized
    (Nothing, Just ft) -> return (ft, Nothing)  -- if method not found, return function
    (Nothing, Nothing) -> throwError $ NameError ("function " ++ fn ++ " wasn't declared") c

methodLookup :: Maybe ClassName -> FunctionName -> Cursor -> StaticAnalysisMonadT (Maybe (TypeValue, ClassName))
methodLookup methodLookupClass methodName c = do
  let
    findMethodInHierarchy :: ClassName -> StaticAnalysisMonadT (Maybe (TypeValue, ClassName))
    findMethodInHierarchy className = do
      ClassHeader superclass _ _ methods <- getClassHeader className c
      case (Map.lookup methodName methods, superclass) of
        (Just t, _) -> return (Just (t, className))
        (Nothing, Just superclassName) -> findMethodInHierarchy superclassName
        (Nothing, Nothing) -> return Nothing
  case methodLookupClass of
    Just className -> findMethodInHierarchy className
    Nothing -> return Nothing

typeMismatchError :: TypeValue -> TypeValue -> Cursor -> Error
typeMismatchError expectedType actualType =
  TypeError (concat ["type mismatch: expected ", show expectedType, ", but got ", show actualType])

isSubtypeOf :: TypeValue -> TypeValue -> StaticAnalysisMonadT Bool
isSubtypeOf (Class c (Ident subtypeName)) supertype@(Class _ (Ident supertypeName)) =
  if subtypeName == supertypeName
    then return True
    else do
      subtypeHeader <- getClassHeader subtypeName c
      case subtypeHeader of
        ClassHeader (Just subtypeParentName) _ _ _ -> isSubtypeOf (Class c (Ident subtypeParentName)) supertype
        _ -> return False
isSubtypeOf _ _ = return False

checkUsedClassExists :: Cursor -> CType -> StaticAnalysisMonadT ()
checkUsedClassExists c (CClass className) = void (getClassHeader className c)
checkUsedClassExists c (CArray (CClass className)) = void (getClassHeader className c)
checkUsedClassExists _ _ = return ()
