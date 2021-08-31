module StaticAnalyser where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Data.Either (partitionEithers)
import AbsLatte
import CommonDeclarations
import StaticAnalysisCommonDeclarations
import StatementAnalyser (analyseBlock, staticExpressionValue, declareVariableTypes)
import ExpressionAnalyser (insertStringConstant, getClassHeader, checkUsedClassExists)

runStaticAnalysis :: CursorProgram -> IO StaticAnalysisResult
runStaticAnalysis (Program _ functionDefinitions) = do
  let
    r = TypeEnvironment {
      classTypes = Map.empty,
      variableTypes = Map.empty,
      functionTypes = Map.empty,
      variablesIntroducedInBlock = Set.empty,
      returnType = dummyVoid,
      classThis = Nothing
    }
    s = TypeStore {
      variableTypeStore = Map.empty,
      stringConstants = Map.empty
    }
  result <- runStateT (runExceptT (runReaderT (staticAnalysis functionDefinitions) r)) s
  case result of
    (Left err, _) -> return (Left err) -- these two Lefts have different types
    (Right tree, store) -> return (Right (tree, stringConstants store))

staticAnalysis :: [TopDefinition] -> StaticAnalysisMonad
staticAnalysis topDefinitions = do
  topDefs <- mapM getTopDefStructure topDefinitions
  let
    d = dummyCursor
    strType = Str d
    intType = Int d
    voidType = Void d
    (classesList, functionsList) = partitionEithers topDefs
    functionsInFile = Map.fromList functionsList
    classHeaders = Map.fromList classesList
    libraryFunctions = Map.fromList [
      ("printInt", Fun d voidType [intType]),
      ("printString", Fun d voidType [strType]),
      ("error", Fun d voidType []),
      ("readInt", Fun d intType []),
      ("readString", Fun d strType [])]
    functionHeaders = Map.union functionsInFile libraryFunctions
    doublyDefinedFunctions = doublyDefinedKeys functionsList
    doublyDefinedClasses = doublyDefinedKeys classesList
    redefined = filter (`Map.member` functionsInFile) (Map.keys libraryFunctions)
  checkCyclicDependency classHeaders
  insertStringConstant "\\00" 1 -- necessary for default values for string arrays
  unless (mainFunctionPresent functionHeaders) (throwError $ NameError "no valid 'int main()' function" dummyCursor)
  unless (null doublyDefinedFunctions) (throwError $ NameError ("some functions are defined at least twice: " ++ intercalate ", " doublyDefinedFunctions) dummyCursor)
  unless (null doublyDefinedClasses) (throwError $ NameError ("some classes are defined at least twice: " ++ intercalate ", " doublyDefinedClasses) dummyCursor)
  unless (null redefined) (throwError $ NameError ("some library functions are redefined: " ++ intercalate ", " redefined) dummyCursor)
  cTopDefs <- local (\e -> e { classTypes = classHeaders, functionTypes = functionHeaders }) (mapM analyseTopDef topDefinitions)
  let
    (cClasses, cFunctions) = partitionEithers cTopDefs
    cClassHeaders = getCClassHeaders classHeaders
  return (CProgram cClasses cFunctions cClassHeaders)

getCClassHeaders :: ClassHeaders -> CClassHeaders
getCClassHeaders classHeaders = Map.map (getCClassHeader classHeaders) classHeaders

getCClassHeader :: ClassHeaders -> ClassHeader -> CClassHeader
getCClassHeader classes (ClassHeader superclass i attrTypes _) = CClassHeader (i + superclassAttributeCount) attrsList (Map.union attrsMap superclassAttrsMap) where
  ((_, reverseAttrsList), attrsMap) = Map.mapAccum acc (superclassAttributeCount, []) attrTypes
  attrsList = superclassAttrs ++ reverse reverseAttrsList
  acc (indexInObject, attrList) attrType = let cType = getCType attrType
    in ((indexInObject + 1, cType : attrList), (cType, indexInObject))
  superclassAttributeCount = toInteger (length superclassAttrs)
  (superclassAttrs, superclassAttrsMap) = case superclass of
    Just superclassName -> case Map.lookup superclassName classes of
      Just sh -> case getCClassHeader classes sh of
        CClassHeader _ superclassAttrs' superclassAttrsMap' -> (superclassAttrs', superclassAttrsMap')
      Nothing -> error "class header not found"
    Nothing -> ([], Map.empty)

checkCyclicDependency :: ClassHeaders -> StaticAnalysisMonadT ()
checkCyclicDependency classes = do
  let
    checkCycle :: Set.Set ClassName -> ClassName -> StaticAnalysisMonadT Bool
    checkCycle visited cls =
      case (Set.member cls visited, Map.lookup cls classes) of
        (True, _) -> return True
        (False, Just (ClassHeader Nothing _ _ _)) -> return False
        (False, Just (ClassHeader (Just superclass) _ _ _)) -> checkCycle (Set.insert cls visited) superclass
        (False, Nothing) -> throwError $ NameError ("class " ++ cls ++ " wasn't defined") dummyCursor
  classesInLoops <- filterM (checkCycle Set.empty) (Map.keys classes)
  unless
    (null classesInLoops)
    (throwError $
      StaticError
        ("there is a cycle in the inheritance tree involving these classes: " ++ intercalate ", " classesInLoops)
        dummyCursor)

mainFunctionPresent :: FunctionHeaders -> Bool
mainFunctionPresent functionHeaders = case Map.lookup "main" functionHeaders of
  Just (Fun _ (Int _) []) -> True
  _ -> False

getTopDefStructure :: TopDefinition -> StaticAnalysisMonadT (Either (ClassName, ClassHeader) (FunctionName, TypeValue) )
getTopDefStructure (ClsDef c (Ident className) superclass (ClsBody _ clsStmts)) = do
  memberVariablesNested <- mapM getMemberVariables clsStmts
  let
    methods = mapMaybe getMethod clsStmts
    memberVariables = concat memberVariablesNested
    doublyDefinedVariables = doublyDefinedKeys memberVariables
    doublyDefinedMethods = doublyDefinedKeys methods
    memberVariableTypes = map snd memberVariables
    objectSize = sum (map getTypeSize memberVariableTypes)
    maybeSuperclass = case superclass of
      Ext _(Ident scn) -> Just scn
      NoExt _ -> Nothing
  unless (null doublyDefinedVariables) (throwError $ NameError ("some member variables are defined at least twice: " ++ intercalate ", " doublyDefinedVariables) c)
  unless (null doublyDefinedMethods) (throwError $ NameError ("some methods are defined at least twice: " ++ intercalate ", " doublyDefinedMethods) c)
  return (Left (className, ClassHeader maybeSuperclass objectSize (Map.fromList memberVariables) (Map.fromList methods)))
getTopDefStructure (FnDef cur type_ (Ident funName) args _) = return (Right (funName, Fun cur type_ (map getArgType args))) where
  getArgType (Arg _ t _) = t

getTypeSize :: TypeValue -> Integer
getTypeSize Int{} = 4
getTypeSize Str{} = 8
getTypeSize Bool{} = 1
getTypeSize Array{} = 8
getTypeSize Class{} = 8
getTypeSize Void{} = error "void doesn't have a size"
getTypeSize Fun{} = error "function doesn't have a size"

getMemberVariables :: ClassStatement -> StaticAnalysisMonadT [(VariableName, TypeValue)]
getMemberVariables (AttrDef c type_ items) = do
  when (type_ == dummyVoid) (throwError $ TypeError "member variable can't have type void" c)
  when (type_ == Array dummyCursor (Void dummyCursor)) (throwError $ TypeError "member variable can't have type void" c)
  let
    getMemberVariableName :: Item Cursor -> StaticAnalysisMonadT String
    getMemberVariableName varDecl = case varDecl of
      NoInit _ (Ident "self") -> throwError $ StaticError "member variable can't be called \"self\"" c
      NoInit _ (Ident i) -> return i
      Init ic _ _ -> throwError $ StaticError "member variable can't have a default value" ic
  memberVariableNames <- mapM getMemberVariableName items
  return [(varName, type_) | varName <- memberVariableNames]
getMemberVariables MetDef{} = return []

getMethod :: ClassStatement -> Maybe (FunctionName, TypeValue)
getMethod AttrDef{} = Nothing
getMethod (MetDef c type_ (Ident name) args _) = Just (name, Fun c type_ (map getArgType args)) where
  getArgType (Arg _ t _) = t

analyseTopDef :: TopDefinition -> StaticAnalysisMonadT (Either CClsDef CFnDef)
analyseTopDef (ClsDef c (Ident className) superclass (ClsBody _ clsStmts)) = do
  when (className == "self") (throwError $ NameError "class can't be called \"self\"" c)
  _ <- case superclass of
    Ext _(Ident scn) -> do
      _ <- getClassHeader scn c  -- check if superclass exists
      return (Just scn)
    NoExt _ -> return Nothing
  members <- mapM (analyseClassStatement className) clsStmts
  let
    (memberVariablesNested, memberMethods) = partitionEithers members
    memberVariables = concat memberVariablesNested
  return (Left (CClsDef className memberVariables memberMethods))
analyseTopDef (FnDef c retType (Ident fnName) args block) = do
  let
    modifyEnvironment e = e { classThis = Nothing }
  (retCType, cArgs, bodyCBlock) <- local modifyEnvironment (analyseFunction c retType args block)
  return (Right (CFnDef retCType fnName cArgs bodyCBlock))

analyseFunction :: Cursor -> TypeValue -> [Arg Cursor] -> CursorBlock -> StaticAnalysisMonadT (CType, [CArg], CBlock)
analyseFunction c retType args block = do
  let
    s = Map.empty
    (varTypes, varNames) = unzip $ map (\(Arg _ type_ (Ident vn)) -> (type_, vn)) args
    (locs, s') = generateLocationsForValues s varTypes
    modifyEnvironment = declareVariableTypes varNames locs . (\e -> e { returnType = retType })
    badArrayArgument (Array _ (Array _ _)) = True
    badArrayArgument _ = False
  mapM_ (checkUsedClassExists c . getCType) varTypes
  modify (\st -> st { variableTypeStore = s'})
  unless (retType == dummyVoid || checkReturnsPresent block) (throwError $ StaticError "function has a path without return" c)
  unless (null $ findSame varNames) (throwError $ NameError "two parameters with the same name" c)
  when (dummyVoid `elem` varTypes) (throwError $ TypeError "parameters cannot have void type" c)
  when (any badArrayArgument varTypes || badArrayArgument retType) (throwError $ TypeError "only one-dimensional arrays allowed" c)
  bodyCBlock <- local modifyEnvironment (analyseBlock block)
  let
    getCArg (Arg _ type_ (Ident vn)) = CArg (getCType type_) vn
  return (getCType retType, zipWith getCArg args locs, bodyCBlock)

analyseClassStatement :: ClassName -> ClassStatement -> StaticAnalysisMonadT (Either [CMemVar] CMetDef)
analyseClassStatement _ (AttrDef c type_ items) = do
  let
    cType = getCType type_
    getMemberVariable :: Item Cursor -> StaticAnalysisMonadT CMemVar
    getMemberVariable varDecl = case varDecl of
      NoInit _ (Ident i) -> return (CMemVar cType i)
      Init{} -> error "member variable default value"
  checkUsedClassExists c cType
  memberVariables <- mapM getMemberVariable items
  return (Left memberVariables)
analyseClassStatement className (MetDef c retType (Ident metName) args body) = do
  let
    modifyEnvironment e = e { classThis = Just className }
  (retCType, cArgs, bodyCBlock) <- local modifyEnvironment (analyseFunction c retType args body)
  return (Right (CMetDef retCType metName cArgs bodyCBlock))

-------------------------------- RETURNS
checkReturnsPresent :: CursorBlock -> Bool
checkReturnsPresent (Block _ stmts) = any checkStatementReturns stmts

checkStatementReturns :: Statement -> Bool
checkStatementReturns (BStmt _ block) = checkReturnsPresent block
checkStatementReturns VRet{} = True
checkStatementReturns Ret{} = True
checkStatementReturns (Cond c e stmt) = checkStatementReturns (CondElse c e stmt (Empty dummyCursor))
checkStatementReturns (CondElse _ e stmt1 stmt2) =
  let
    val = staticExpressionValue e
  in case val of
    Just v -> if v then checkStatementReturns stmt1 else checkStatementReturns stmt2
    Nothing -> checkStatementReturns stmt1 && checkStatementReturns stmt2
checkStatementReturns (While c e stmt) = checkStatementReturns (CondElse c e stmt (Empty dummyCursor))
checkStatementReturns _ = False
