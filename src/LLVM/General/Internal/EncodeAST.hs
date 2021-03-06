{-# LANGUAGE
  GeneralizedNewtypeDeriving,
  FlexibleContexts,
  FlexibleInstances,
  MultiParamTypeClasses,
  UndecidableInstances
  #-}

module LLVM.General.Internal.EncodeAST where

import Control.Exception
import Control.Monad.State
import Control.Monad.Phased
import Control.Monad.Error
import Control.Monad.AnyCont

import Foreign.Ptr
import Foreign.C

import Data.Map (Map)
import qualified Data.Map as Map

import qualified LLVM.General.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.General.Internal.FFI.Builder as FFI
import qualified LLVM.General.Internal.FFI.Type as FFI

import qualified LLVM.General.AST as A

import LLVM.General.Internal.Context
import LLVM.General.Internal.Coding
import LLVM.General.Internal.String ()

data EncodeState = EncodeState { 
      encodeStateBuilder :: Ptr FFI.Builder,
      encodeStateContext :: Context,
      encodeStateLocals :: Map A.Name (Ptr FFI.Value),
      encodeStateGlobals :: Map A.Name (Ptr FFI.GlobalValue),
      encodeStateAllBlocks :: Map (A.Name, A.Name) (Ptr FFI.BasicBlock),
      encodeStateBlocks :: Map A.Name (Ptr FFI.BasicBlock),
      encodeStateMDNodes :: Map A.MetadataNodeID (Ptr FFI.MDNode),
      encodeStateNamedTypes :: Map A.Name (Ptr FFI.Type)
    }

newtype EncodeAST a = EncodeAST { unEncodeAST :: AnyContT (PhasedT (ErrorT String (StateT EncodeState IO))) a }
    deriving (
       Functor,
       Monad,
       MonadIO,
       MonadState EncodeState,
       MonadPhased,
       MonadError String
     )

instance MonadAnyCont IO EncodeAST where
  anyContToM = EncodeAST . anyContIOToM
  scopeAnyCont = EncodeAST . scopeAnyCont . unEncodeAST

lookupNamedType :: A.Name -> EncodeAST (Ptr FFI.Type)
lookupNamedType n = do
  t <- gets $ Map.lookup n . encodeStateNamedTypes
  maybe (fail $ "reference to undefined type: " ++ show n) return t

defineType :: A.Name -> Ptr FFI.Type -> EncodeAST ()
defineType n t = modify $ \s -> s { encodeStateNamedTypes = Map.insert n t (encodeStateNamedTypes s) }

runEncodeAST :: Context -> EncodeAST a -> IO (Either String a)
runEncodeAST context@(Context ctx) (EncodeAST a) = 
    bracket (FFI.createBuilderInContext ctx) FFI.disposeBuilder $ \builder -> do
      let initEncodeState = EncodeState { 
              encodeStateBuilder = builder,
              encodeStateContext = context,
              encodeStateLocals = Map.empty,
              encodeStateGlobals = Map.empty,
              encodeStateAllBlocks = Map.empty,
              encodeStateBlocks = Map.empty,
              encodeStateMDNodes = Map.empty,
              encodeStateNamedTypes = Map.empty
            }
      flip evalStateT initEncodeState . runErrorT . runPhasedT . flip runAnyContT return $ a

withName :: A.Name -> (CString -> IO a) -> IO a
withName (A.Name n) = withCString n
withName (A.UnName _) = withCString ""

instance MonadAnyCont IO m => EncodeM m A.Name CString where
  encodeM (A.Name n) = encodeM n
  encodeM _ = encodeM ""

-- contain modifications to the local part of the encode state - in this case all those except
-- those to encodeStateAllBlocks
encodeScope :: EncodeAST a -> EncodeAST a
encodeScope (EncodeAST x) = 
  EncodeAST . mapAnyContT pScope $ x -- get inside the boring wrappers down to the phasing
  where pScope (PhasedT x) = PhasedT $ do
          let s0 `withLocalsFrom` s1 = s0 { 
                 encodeStateLocals = encodeStateLocals s1,
                 encodeStateBlocks = encodeStateBlocks s1
                }
          state <- get -- save the state
          a <- x
          state' <- get -- get the modified state
          put $ state' `withLocalsFrom` state -- revert the local part
          -- Finally here's the fun bit - in the Left case where we're coming back to a deferment point,
          -- prepend an action which reinstates the local state, but re-wrap with pScope to continue
          -- containment.
          return $ either (Left . pScope . (modify (`withLocalsFrom` state') >>)) Right a


define :: (Ord n, FFI.DescendentOf p v) => 
          (EncodeState -> Map n (Ptr p))
          -> (Map n (Ptr p) -> EncodeState -> EncodeState)
          -> n
          -> Ptr v
          -> EncodeAST ()
define r w n v = modify $ \b -> w (Map.insert n (FFI.upCast v) (r b)) b

defineLocal :: FFI.DescendentOf FFI.Value v => A.Name -> Ptr v -> EncodeAST ()
defineLocal = define encodeStateLocals (\m b -> b { encodeStateLocals = m })

defineGlobal :: FFI.DescendentOf FFI.GlobalValue v => A.Name -> Ptr v -> EncodeAST ()
defineGlobal = define encodeStateGlobals (\m b -> b { encodeStateGlobals = m })

defineMDNode :: A.MetadataNodeID -> Ptr FFI.MDNode -> EncodeAST ()
defineMDNode = define encodeStateMDNodes (\m b -> b { encodeStateMDNodes = m })

refer :: (Show n, Ord n) => (EncodeState -> Map n (Ptr p)) -> String -> n -> EncodeAST (Ptr p)
refer r m n = do
  mop <- gets $ Map.lookup n . r
  maybe (fail $ "reference to undefined " ++ m ++ ": " ++ show n) return mop

referLocal = refer encodeStateLocals "local"
referGlobal = refer encodeStateGlobals "global"
referMDNode = refer encodeStateMDNodes "metadata node"

defineBasicBlock :: A.Name -> A.Name -> Ptr FFI.BasicBlock -> EncodeAST ()
defineBasicBlock fn n b = modify $ \s -> s {
  encodeStateBlocks = Map.insert n b (encodeStateBlocks s),
  encodeStateAllBlocks = Map.insert (fn, n) b (encodeStateAllBlocks s)
}

instance EncodeM EncodeAST A.Name (Ptr FFI.BasicBlock) where
  encodeM = refer encodeStateBlocks "block"

getBlockForAddress :: A.Name -> A.Name -> EncodeAST (Ptr FFI.BasicBlock)
getBlockForAddress fn n = refer encodeStateAllBlocks "blockaddress" (fn, n)

