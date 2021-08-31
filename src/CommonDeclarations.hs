module CommonDeclarations where

import AbsLatte

import System.IO (hPutStrLn, stderr)
import Control.Monad (when)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.FilePath as FilePath

type Cursor = (Int, Int)
type Expression = Expr Cursor
type Statement = Stmt Cursor
type ClassStatement = ClsStmt Cursor
type Member = Mem Cursor
type TopDefinition = TopDef Cursor
type CursorProgram = Program Cursor
type CursorBlock = Block Cursor
type TypeValue = Type Cursor
type Location = Int
type StringConstants = Map.Map String (String, Integer)

data Value = Register String | IntValue Integer | BoolValue Bool | PtrValue String | VoidValue

instance Show Value where
  show (Register r) = "%" ++ r
  show (IntValue v) = show v
  show (BoolValue True) = "1"
  show (BoolValue False) = "0"
  show (PtrValue v) = v
  show VoidValue = error "trying to use void value"

maxPower :: Integer
maxPower = 31

dummyCursor :: Cursor
dummyCursor = (0, 0)

dummyVoid :: TypeValue
dummyVoid = Void dummyCursor
dummyInt :: TypeValue
dummyInt = Int dummyCursor

printErrorString :: String -> IO ()
printErrorString = hPutStrLn stderr

printSuccessString :: String -> IO ()
printSuccessString = putStrLn

printError :: Error -> IO ()
printError e = do
  printErrorString "ERROR"
  (printErrorString . formatError) e

data Error =
  SyntaxError String |
  ArithmeticError String Cursor |
  TypeError String Cursor |
  NameError String Cursor |
  StaticError String Cursor |
  RuntimeError String Cursor

formatError' :: String -> String -> Cursor -> String
formatError' e s (l, c) = concat [e, " at line ",
                                  show l, ", column ",
                                  show c, ": ", s]

formatError :: Error -> String
formatError (SyntaxError s) = s
formatError (ArithmeticError s c) = formatError' "ArithmeticError" s c
formatError (TypeError s c) = formatError' "TypeError" s c
formatError (NameError s c) = formatError' "NameError" s c
formatError (StaticError s c) = formatError' "StaticError" s c
formatError (RuntimeError s c) = formatError' "RuntimeError" s c

getStmtCursor :: Statement -> Cursor
getStmtCursor (Empty c) = c
getStmtCursor (BStmt c _) = c
getStmtCursor (Decl c _ _) = c
getStmtCursor (Ass c _ _) = c
getStmtCursor (AttrAss c _ _ _) = c
getStmtCursor (Incr c _) = c
getStmtCursor (AttrIncr c _ _) = c
getStmtCursor (Decr c _) = c
getStmtCursor (AttrDecr c _ _) = c
getStmtCursor (Ret c _) = c
getStmtCursor (VRet c) = c
getStmtCursor (Cond c _ _) = c
getStmtCursor (CondElse c _ _ _) = c
getStmtCursor (While c _ _) = c
getStmtCursor (ForEach c _ _ _ _) = c
getStmtCursor (SExp c _) = c


getExprCursor :: Expression -> Cursor
getExprCursor e =
  case e of
    EVar c _ -> c
    ELitInt c _ -> c
    ELitTrue c -> c
    ELitFalse c -> c
    EApp c _ _ -> c
    EString c _ -> c
    ELitCls c _ -> c
    ELitNull c _ -> c
    ELitArr c _ _ -> c
    ENameAttr c _ _ -> c
    EExprAttr c _ _ -> c
    Neg c _ -> c
    Not c _ -> c
    EMul c _ _ _ -> c
    EAdd c _ _ _ -> c
    ERel c _ _ _ -> c
    EAnd c _ _ -> c
    EOr c _ _ -> c

freshLocation :: Map.Map k v -> Location
freshLocation = Map.size

mapInsertManyValues :: Ord a => [a] -> [b] -> Map.Map a b -> Map.Map a b
mapInsertManyValues keys vals kvMap = foldr addOneValue kvMap pairs where
  addOneValue (k, v) = Map.insert k v
  pairs = zip keys vals

setInsertManyValues :: Ord a => Set.Set a -> [a] -> Set.Set a
setInsertManyValues = foldr Set.insert

generateLocationsForValues :: Map.Map Location v -> [v] -> ([Location], Map.Map Location v)
generateLocationsForValues s =
  foldr generateSingleLocation ([], s) where
    generateSingleLocation val (locs, currentS) =
      let newLoc = freshLocation currentS in
        (newLoc:locs, Map.insert newLoc val currentS)

findSame :: (Eq a) => [a] -> [a]
findSame [] = []
findSame (x:xs)
  | elem x xs && notElem x rest = x:rest
  | otherwise = rest
  where rest = findSame xs

doublyDefinedKeys :: (Eq a) => [(a, b)] -> [a]
doublyDefinedKeys = findSame . map fst

-- CODE GENERATOR INPUT TYPES
data CProgram = CProgram [CClsDef] [CFnDef] CClassHeaders
type CIdent = String

type ClassName = String
type VariableName = String
data CClassHeader = CClassHeader Integer [CType] (Map.Map VariableName (CType, Integer))
  deriving (Eq, Ord, Show, Read)
type CClassHeaders = Map.Map ClassName CClassHeader

data CClsDef = CClsDef CIdent [CMemVar] [CMetDef]
  deriving (Eq, Ord, Show, Read)
data CFnDef = CFnDef CType CIdent [CArg] CBlock
  deriving (Eq, Ord, Show, Read)

data CMemVar = CMemVar CType CIdent
  deriving (Eq, Ord, Show, Read)
data CMetDef = CMetDef CType CIdent [CArg] CBlock
  deriving (Eq, Ord, Show, Read)
data CArg = CArg CType CIdent Location
  deriving (Eq, Ord, Show, Read)

newtype CBlock = CBlock [CStatement]
  deriving (Eq, Ord, Show, Read)

data CVariable = CLocalVariable CIdent Location | CAttributeVariable ClassName CIdent | CSelf
  deriving (Eq, Ord, Show, Read)

data CStatement
    = CEmpty
    | CBStatement CBlock
    | CDecl CType [CItem]
    | CAss CVariable CExpression
    -- | CIndAss CVariable CExpression CExpression
    | CAttrAss CType CVariable [CDMem] CExpression
    | CIncr CVariable
    | CDecr CVariable
    | CRet CExpression
    | CVRet
    | CCond CExpression CStatement
    | CCondElse CExpression CStatement CStatement
    | CWhile CExpression CStatement
    | CForEach CType CIdent Location CExpression CStatement
    | CSExp CExpression
  deriving (Eq, Ord, Show, Read)

data CItem = CItem CIdent Location CExpression
  deriving (Eq, Ord, Show, Read)

data CType
    = CInt | CStr | CBool | CVoid | CArray CType | CClass String
  deriving (Eq, Ord, Show, Read)

data CDMem = CDMemVar ClassName CIdent | CDMemMet ClassName ClassName CType CIdent [CExpression] | CDMemInd CType CExpression | CDLength
  deriving (Eq, Ord, Show, Read)

data CExpression
    = CVar CType CVariable
    | CLitInt Integer
    | CLitTrue
    | CLitFalse
    | CLitNull String
    | CApp CType CIdent [CExpression]
    | CMetApp ClassName ClassName CType CIdent [CExpression]
    | CString String
    | CLitArr CType CExpression
    | CLitCls CIdent
    | CCast CType CExpression CType
    -- | CExprInd CType CExpression CExpression
    | CArrayLength CExpression
    | CExprAttr CType CExpression [CDMem]
    | CNeg CExpression
    | CNot CExpression
    | CMulInt CExpression CMulOp CExpression
    | CAddString CExpression CExpression
    | CAddInt CExpression CAddOp CExpression
    | CRel CExpression CRelOp CExpression
    | CAnd CExpression CExpression
    | COr CExpression CExpression
  deriving (Eq, Ord, Show, Read)

data CMulOp = CTimes | CDiv | CMod
  deriving (Eq, Ord, Show, Read)
data CAddOp = CPlus | CMinus
  deriving (Eq, Ord, Show, Read)
data CRelOp = CLTH | CLE | CGTH | CGE | CEQU | CNE
  deriving (Eq, Ord, Show, Read)

getCType :: TypeValue -> CType
getCType Int{} = CInt
getCType Str{} = CStr
getCType Bool{} = CBool
getCType Void{} = CVoid
getCType (Array _ t) = CArray (getCType t)
getCType (Class _ (Ident name)) = CClass name
getCType Fun{} = error "can't get ctype of a function"

getCTypeSize :: CType -> Integer
getCTypeSize CInt = 4
getCTypeSize CStr = 8
getCTypeSize CBool = 1
getCTypeSize CArray{} = 8
getCTypeSize CClass{} = 8
getCTypeSize CVoid = error "void doesn't have a size"

getExpressionCType :: CExpression -> CType
getExpressionCType (CVar ct _) = ct
getExpressionCType CLitInt{} = CInt
getExpressionCType CLitTrue{} = CBool
getExpressionCType CLitFalse{} = CBool
getExpressionCType (CLitNull className) = CClass className
getExpressionCType (CApp ct _ _) = ct
getExpressionCType (CMetApp _ _ ct _ _) = ct
getExpressionCType CString{} = CStr
getExpressionCType (CLitArr ct _) = CArray ct
getExpressionCType (CLitCls className) = CClass className
getExpressionCType (CCast _ _ ct) = ct
-- getExpressionCType (CExprInd ct _ _) = ct
getExpressionCType CArrayLength{} = CInt
getExpressionCType (CExprAttr ct _ _) = ct
getExpressionCType CNeg{} = CInt
getExpressionCType CNot{} = CBool
getExpressionCType CMulInt{} = CInt
getExpressionCType CAddString{} = CStr
getExpressionCType CAddInt{} = CInt
getExpressionCType CRel{} = CBool
getExpressionCType CAnd{} = CBool
getExpressionCType COr{} = CBool

getLLVMType :: CType -> String
getLLVMType CInt = "i32"
getLLVMType CStr = "i8*"
getLLVMType CBool = "i1"
getLLVMType CArray{} = "%_Array*"
getLLVMType (CClass className) = getLLVMClassName className ++ "*"
getLLVMType CVoid = "void"

getLLVMClassName :: ClassName -> String
getLLVMClassName className = "%$class$" ++ className

getDefaultExpression :: TypeValue -> Expression
getDefaultExpression (Int c) = ELitInt c 0
getDefaultExpression (Str c) = EString c ""
getDefaultExpression (Bool c) = ELitFalse c
getDefaultExpression (Array c t) = ELitArr c t (ELitInt c 0)
getDefaultExpression (Class c name) = ELitNull c (EVar c name)
getDefaultExpression Void{} = error "no default expression for void type"
getDefaultExpression Fun{} = error "no default expression for function type"

getDefaultCExpression :: CType -> CExpression
getDefaultCExpression CInt = CLitInt 0
getDefaultCExpression CStr = CString "\\00"
getDefaultCExpression CBool = CLitFalse
getDefaultCExpression (CClass t) = CLitNull t
getDefaultCExpression (CArray t) = CLitArr t (CLitInt 0)
getDefaultCExpression CVoid = error "no default expression for void type"

getCMulOp :: MulOp a -> CMulOp
getCMulOp Times{} = CTimes
getCMulOp Div{} = CDiv
getCMulOp Mod{} = CMod

getCAddOp :: AddOp a -> CAddOp
getCAddOp Plus{} = CPlus
getCAddOp Minus{} = CMinus

getRelOp :: RelOp a -> CRelOp
getRelOp LTH{} = CLTH
getRelOp LE{} = CLE
getRelOp GTH{} = CGTH
getRelOp GE{} = CGE
getRelOp EQU{} = CEQU
getRelOp NE{} = CNE

parseArguments :: [String] -> String -> IO (String, String, String)
parseArguments args extension = do
  inputFile <-
    case args of
      (s:_) -> return s
      [] -> error "no file given"
  let
    fileExtension = FilePath.takeExtension inputFile
    directory = FilePath.takeDirectory inputFile
    fileBaseName = FilePath.takeBaseName inputFile
    outputFile = directory FilePath.</> fileBaseName ++ extension
  when (fileExtension /= ".lat") (error "wrong file extension")
  return (inputFile, outputFile, fileBaseName)
