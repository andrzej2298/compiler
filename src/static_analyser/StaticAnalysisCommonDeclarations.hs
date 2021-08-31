module StaticAnalysisCommonDeclarations where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import CommonDeclarations

type StaticAnalysisMonadT a = ReaderT TypeEnvironment (ExceptT Error
                                                      (StateT TypeStore IO)) a
type StaticAnalysisMonad = StaticAnalysisMonadT CProgram
type StaticAnalysisResult = Either Error (CProgram, StringConstants)
type FunctionName = String
data ClassHeader = ClassHeader (Maybe ClassName) Integer (Map.Map VariableName TypeValue) (Map.Map FunctionName TypeValue)
  deriving (Eq, Ord, Show, Read)
type ClassHeaders = Map.Map ClassName ClassHeader
type FunctionHeaders = Map.Map FunctionName TypeValue
type VariableTypeEnvironment = Map.Map VariableName Location


data TypeEnvironment = TypeEnvironment {
  classTypes :: ClassHeaders,
  variableTypes :: VariableTypeEnvironment,
  functionTypes :: FunctionHeaders,
  variablesIntroducedInBlock :: Set.Set VariableName,
  returnType :: TypeValue,
  classThis :: Maybe String
}

data TypeStore = TypeStore { variableTypeStore :: Map.Map Location TypeValue, stringConstants :: StringConstants }
