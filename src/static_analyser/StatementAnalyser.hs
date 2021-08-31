module StatementAnalyser where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import StaticAnalysisCommonDeclarations
import CommonDeclarations
import AbsLatte
import ExpressionAnalyser (analyseExpression, getVarTypeAss, getVarTypeNoAss,
                           typeMismatchError, dereferenceAttributes, isSubtypeOf, checkUsedClassExists)

analyseBlock :: CursorBlock -> StaticAnalysisMonadT CBlock

analyseBlock (Block c (Decl dc type_ (varDecl:varDecls) : stmts)) = do
  when (type_ == dummyVoid) (throwError $ TypeError "variable can't have type void" c)
  s <- gets variableTypeStore
  let
    cType = getCType type_
    loc = freshLocation s
    s' = Map.insert loc type_ s
  (varName, assignedType, currentCItem) <- do
    let
      (expression, vn) = case varDecl of
        NoInit _ (Ident i) -> (getDefaultExpression type_, i)
        Init _ (Ident i) e -> (e, i)
    (t, cExpression) <- analyseExpression expression
    let castCExpression = if t == type_ then cExpression else CCast (getCType t) cExpression (getCType type_)
    return (vn, t, CItem vn loc castCExpression)
  checkUsedClassExists dc cType
  declared <- alreadyDeclared varName
  validSubtype <- isSubtypeOf assignedType type_
  when declared (throwError $ NameError ("variable " ++ varName ++ " already declared") dc)
  when (assignedType /= type_ && not validSubtype) (throwError $ typeMismatchError type_ assignedType dc)
  modify (\st -> st { variableTypeStore = s' })
  CBlock (CDecl _ restCItems : cStmts) <- local (declareVariableTypes [varName] [loc]) (analyseBlock (Block c (Decl dc type_ varDecls : stmts)))
  return (CBlock (CDecl cType (currentCItem:restCItems) : cStmts))

analyseBlock (Block c (Decl _ type_ [] : stmts)) = do
  CBlock restCStmts <- analyseBlock (Block c stmts)
  return $ CBlock (CDecl (getCType type_) [] : restCStmts)

analyseBlock (Block c (stmt:stmts)) = do
  currentCStmt <- analyseStatement stmt
  CBlock restOfCBlock <- analyseBlock (Block c stmts)
  return $ CBlock (currentCStmt:restOfCBlock)

analyseBlock (Block _ []) = return $ CBlock []

analyseStatement :: Statement -> StaticAnalysisMonadT CStatement

analyseStatement Empty{} = return CEmpty

analyseStatement (BStmt _ block) = do
  blockCBody <- local (\e -> e { variablesIntroducedInBlock = Set.empty }) (analyseBlock block)
  return $ CBStatement blockCBody

analyseStatement (Decl c _ _) = throwError $ StaticError "variable declaration not allowed in single then/else/while/for statement" c

analyseStatement (Ass ac (Ident var) e) = do
  (declaredType, assCVar) <- getVarTypeAss var ac
  (expressionType, cExpression) <- analyseExpression e
  validSubtype <- isSubtypeOf expressionType declaredType
  when (expressionType /= declaredType && not validSubtype) (throwError $ typeMismatchError declaredType expressionType ac)
  let castCExpression = if expressionType == declaredType then cExpression else CCast (getCType expressionType) cExpression (getCType declaredType)
  return (CAss assCVar castCExpression)

analyseStatement (AttrAss c (Ident variableName) attrs e) = do
  (expressionType, cExpression) <- analyseExpression e
  (variableType, objCVar) <- getVarTypeNoAss variableName c
  (assignedValueType, dereferenceList) <- dereferenceAttributes variableType attrs True
  validSubtype <- isSubtypeOf expressionType assignedValueType
  when (assignedValueType /= expressionType && not validSubtype) (throwError $ typeMismatchError assignedValueType expressionType c)
  let castExpression = if assignedValueType == expressionType then cExpression else CCast (getCType expressionType) cExpression (getCType assignedValueType)
  return (CAttrAss (getCType variableType) objCVar dereferenceList castExpression)

analyseStatement (Incr c (Ident varName)) = do
  (t, cVar) <- getVarTypeAss varName c
  let expectedType = Int dummyCursor
  when (t /= expectedType) (throwError $ TypeError ("can't increment " ++ show t) c)
  return (CIncr cVar)

analyseStatement (AttrIncr c objIdent attrs) =
  analyseStatement (AttrAss c objIdent attrs (EAdd c (EExprAttr c (EVar c objIdent) attrs) (Plus c) (ELitInt c 1)))

analyseStatement (Decr c (Ident varName)) = do
  (t, cVar) <- getVarTypeAss varName c
  let expectedType = Int dummyCursor
  when (t /= expectedType) (throwError $ TypeError ("can't decrement " ++ show t) c)
  return (CDecr cVar)

analyseStatement (AttrDecr c objIdent attrs) =
  analyseStatement (AttrAss c objIdent attrs (EAdd c (EExprAttr c (EVar c objIdent) attrs) (Minus c) (ELitInt c 1)))

analyseStatement (Ret c e) = do
  (t, ce) <- analyseExpression e
  expectedReturnType <- asks returnType
  validSubtype <- isSubtypeOf t expectedReturnType
  when (t == dummyVoid) (throwError $ TypeError "trying to return void" c)
  when (t /= expectedReturnType && not validSubtype) (throwError $ TypeError ("invalid return type: got " ++ show t ++ ", expected " ++ show expectedReturnType) c)
  let castCExpression = if t == expectedReturnType then ce else CCast (getCType t) ce (getCType expectedReturnType)
  return (CRet castCExpression)

analyseStatement (VRet c) = do
  expectedReturnType <- asks returnType
  when (expectedReturnType /= dummyVoid) (throwError $ TypeError ("returning void, but function's return type declared to be " ++ show expectedReturnType) c)
  return CVRet

analyseStatement (Cond c expr stmt) = do
  cExpression <- analyseConditionalStatement expr c
  cStmt <- analyseStatement stmt
  case staticExpressionValue expr of
    Just True -> return cStmt
    Just False -> return CEmpty
    Nothing -> return (CCond cExpression cStmt)

analyseStatement (CondElse c expr stmt1 stmt2) = do
  cExpression <- analyseConditionalStatement expr c
  cStmt1 <- analyseStatement stmt1
  cStmt2 <- analyseStatement stmt2
  case staticExpressionValue expr of
    Just True -> return cStmt1
    Just False -> return cStmt2
    Nothing -> return (CCondElse cExpression cStmt1 cStmt2)

analyseStatement (While c expr stmt) = do
  cExpression <- analyseConditionalStatement expr c
  cStmt <- analyseStatement stmt
  case staticExpressionValue expr of
    Just False -> return CEmpty
    _ -> return (CWhile cExpression cStmt)

analyseStatement (ForEach c varType (Ident varName) e loopBody) = do
  (t, ce) <- analyseExpression e
  s <- gets variableTypeStore
  let
    cType = getCType varType
    loc = freshLocation s
    s' = Map.insert loc varType s
  modify (\st -> st { variableTypeStore = s' })
  cLoopBody <- local (declareVariableTypes [varName] [loc]) (analyseStatement loopBody)
  case t of
    (Array _ arrayType) -> do
      when (varType /= arrayType) (throwError (typeMismatchError arrayType varType c))
      return (CForEach cType varName loc ce cLoopBody)
    _ -> throwError $ TypeError "a for loop can iterate only over arrays" c

analyseStatement (SExp _ e) = do
  (_, cExpression) <- analyseExpression e
  return $ CSExp cExpression

analyseConditionalStatement :: Expression -> Cursor -> StaticAnalysisMonadT CExpression
analyseConditionalStatement expr c = do
  (t, cExpression) <- analyseExpression expr
  let expectedType = Bool dummyCursor
  when (t /= expectedType) (throwError $ TypeError ("conditional expression can't be of type " ++ show t) c)
  return cExpression

declareVariableTypes :: [VariableName] -> [Location] -> TypeEnvironment -> TypeEnvironment
declareVariableTypes xs ls env = env {
  variableTypes = mapInsertManyValues xs ls (variableTypes env),
  variablesIntroducedInBlock = setInsertManyValues (variablesIntroducedInBlock env) xs}

alreadyDeclared :: VariableName -> StaticAnalysisMonadT Bool
alreadyDeclared x = do
  vars <- asks variablesIntroducedInBlock
  return $ Set.member x vars

staticExpressionValue :: Expression -> Maybe Bool
staticExpressionValue (ELitTrue _) = Just True
staticExpressionValue (ELitFalse _) = Just False
staticExpressionValue (Not _ e) = case staticExpressionValue e of
  (Just v) -> Just $ not v
  _ -> Nothing
staticExpressionValue (EAnd _ e1 e2) = case (staticExpressionValue e1, staticExpressionValue e2) of
  (Just v1, Just v2) -> Just $ v1 && v2
  _ -> Nothing
staticExpressionValue (EOr _ e1 e2) = case (staticExpressionValue e1, staticExpressionValue e2) of
  (Just v1, Just v2) -> Just $ v1 || v2
  _ -> Nothing
staticExpressionValue _ = Nothing
