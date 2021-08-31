module StatementGenerator where

import Control.Monad.State
import CodeGenerationCommonDeclarations
import CommonDeclarations

import ExpressionGenerator (generateExpression, dereferenceNestedClassMember)

generateStatement :: CStatement -> CodeGenerationMonad

generateStatement CEmpty = return ()

generateStatement (CBStatement (CBlock stmts)) = mapM_ generateStatement stmts

generateStatement (CDecl t items) = do
  appendIndentDebug "; CDecl"
  let
    getDecl :: CItem -> CodeGenerationMonad
    getDecl (CItem ident loc e) = do
      varRegister <- getVarRegister (CLocalVariable ident loc)
      emitAlloca varRegister t
      reg <- generateExpression e
      emitStore t reg varRegister
  mapM_ getDecl items

generateStatement (CAss cVariable e) = do
  appendIndentDebug "; CAss"
  reg <- generateExpression e
  varRegister <- getVarRegister cVariable
  emitStore (getExpressionCType e) reg varRegister

generateStatement (CAttrAss cType cVariable attrs assignedExpression) = do
  appendIndentDebug "; CAttrAss"
  assignedValue <- generateExpression assignedExpression
  objectExpression <- generateExpression (CVar cType cVariable)
  (Just (fieldPointer, _), fieldType) <- dereferenceNestedClassMember objectExpression attrs
  emitStore fieldType assignedValue fieldPointer

generateStatement (CIncr cVariable) = do
  appendIndentDebug "; CIncr"
  generateStatement (CAss cVariable (CAddInt (CVar CInt cVariable) CPlus (CLitInt 1)))

generateStatement (CDecr cVariable) = do
  appendIndentDebug "; CDecr"
  generateStatement (CAss cVariable (CAddInt (CVar CInt cVariable) CMinus (CLitInt 1)))

generateStatement (CRet e) = do
  appendIndentDebug "; CRet"
  reg <- generateExpression e
  retLabel <- gets returnLabel
  retRegister <- gets returnRegister
  emitStore (getExpressionCType e) reg retRegister
  emitBranchUnconditional retLabel

generateStatement CVRet = do
  appendIndentDebug "; CVRet"
  retLabel <- gets returnLabel
  emitBranchUnconditional retLabel

generateStatement (CCond expr stmt) = do
  appendIndentDebug "; CCond"
  condExpr <- generateExpression expr
  labelThen <- getFreeLabel
  labelAfter <- getFreeLabel
  emitBranchIfElse condExpr labelThen labelAfter
  emitLabel labelThen
  generateStatement stmt
  emitBranchUnconditional labelAfter
  emitLabel labelAfter

generateStatement (CCondElse expr stmt1 stmt2) = do
  appendIndentDebug "; CCondElse"
  condExpr <- generateExpression expr
  labelThen <- getFreeLabel
  labelElse <- getFreeLabel
  emitBranchIfElse condExpr labelThen labelElse
  emitLabel labelThen
  generateStatement stmt1
  labelAfter <- getFreeLabel
  emitBranchUnconditional labelAfter
  emitLabel labelElse
  generateStatement stmt2
  emitBranchUnconditional labelAfter
  emitLabel labelAfter

generateStatement (CWhile expr stmt) = do
  appendIndentDebug "; CWhile"
  [labelCondition, labelLoop, labelAfter] <- replicateM 3 getFreeLabel
  emitBranchUnconditional labelCondition
  emitLabel labelCondition
  condExpr <- generateExpression expr
  emitBranchIfElse condExpr labelLoop labelAfter
  emitLabel labelLoop
  generateStatement stmt
  emitBranchUnconditional labelCondition
  emitLabel labelAfter

generateStatement (CForEach cType varName location arrayExpression loopBody) = do
  appendIndentDebug "; CForEach"
  let
    temporaryIndex = "_temporary_index"
    temporaryArrayLength = "_temporary_array_length"
    temporaryArrayVariable = "_temporary_array_variable"
  generateStatement (CDecl (CArray cType) [CItem temporaryArrayVariable location arrayExpression])
  generateStatement (CDecl CInt [CItem temporaryIndex location (CLitInt 0),
                                 CItem temporaryArrayLength location (CArrayLength (CVar (CArray cType) (CLocalVariable temporaryArrayVariable location)))])
  generateStatement (CDecl cType [CItem varName location (getDefaultCExpression cType)])

  generateStatement
    (CWhile
      (CRel (CVar CInt (CLocalVariable temporaryIndex location)) CLTH (CVar CInt (CLocalVariable temporaryArrayLength location)))
      (CBStatement (CBlock
        [CAss
            (CLocalVariable varName location)
            (CExprAttr cType (CVar (CArray cType) (CLocalVariable temporaryArrayVariable location)) [CDMemInd cType (CVar CInt (CLocalVariable temporaryIndex location))]),
         loopBody,
         CIncr (CLocalVariable temporaryIndex location)])))

generateStatement (CSExp e) = void $ generateExpression e
