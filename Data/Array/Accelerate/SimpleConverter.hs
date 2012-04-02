{-# LANGUAGE BangPatterns, GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data.Array.Accelerate.SimpleConverter 
--       ()
       where

-- standard libraries
import Control.Monad
import Control.Applicative ((<$>),(<*>))
-- import Control.Monad.ST                            (ST)
import Data.Bits
import Data.Char                                   (chr, ord)
import Prelude                                     hiding (sum)
import Debug.Trace

import Control.Monad.State.Strict

-- friends
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.Array.Data
import Data.Array.Accelerate.Array.Representation  hiding (sliceIndex)
import Data.Array.Accelerate.Array.Sugar (
  Z(..), (:.)(..), Array(..), Scalar, Vector, Segments)
import Data.Array.Accelerate.Array.Delayed
import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Smart       as Sugar
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.SimpleAST as S

-- TEMP:
import qualified Data.Array.Accelerate.Language as Lang

-- #include "accelerate.h"

--------------------------------------------------------------------------------
-- TEMPORARY -- Testing:

-- dotpAcc :: Vector Float -> Vector Float -> Sugar.Acc (Sugar.Scalar Float)
dotpAcc :: Sugar.Acc (Sugar.Scalar Float)
dotpAcc 
  = let
        xs' = Lang.use $ error "xs: let's not REALLY use this array"
        ys' = Lang.use $ error "ys: let's not REALLY use this array"
    in
     (Lang.zipWith (*) xs' ys')
--    Lang.fold (+) 0 (Lang.zipWith (*) xs' ys')

t1 :: S.Exp
t1 = convert dotpAcc


--------------------------------------------------------------------------------
-- Exposed entrypoints for this module:
--------------------------------------------------------------------------------

-- | Convert the sophisticate Accelerate-internal AST representation
--   into something very simple for external consumption.
convert :: Arrays a => Sugar.Acc a -> S.Exp
convert = runEnvM . convertAcc . Sugar.convertAcc

-- convertAccFun1 = 



--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------


-- We use a simple state monad for keeping track of the environment
type EnvM = State (SimpleEnv,Counter) 
type SimpleEnv = [S.Var]
type Counter = Int

runEnvM m = evalState m ([],0)

-- Evaluate a sub-branch in an extended environment.
-- Returns the name of the fresh variable as well as the result:
withExtendedEnv :: String -> (EnvM b) -> EnvM (S.Var, b )
withExtendedEnv basename branch = do 
  (env,cnt) <- get 
  let newsym = S.var $ basename ++ show cnt
  put (newsym:env, cnt+1) 
  b <- branch
  -- We keep counter-increments from the sub-branch, but NOT the extended env:
  (_,cnt2) <- get 
  put (env,cnt2)
  return (newsym, b)

-- Look up a de bruijn index in the environment:
envLookup :: Int -> EnvM S.Var
envLookup i = do (env,_) <- get
                 if length env > i
                  then return (env !! i)
                  else error$ "Environment did not contain an element "++show i++" : "++show env


--------------------------------------------------------------------------------
-- Convert Accelerate Array-level Expressions
--------------------------------------------------------------------------------

convertAcc :: Delayable a => Acc a -> EnvM S.Exp
convertAcc acc = convertOpenAcc acc 

convertOpenAcc :: Delayable a => OpenAcc aenv a -> EnvM S.Exp
convertOpenAcc (OpenAcc acc) = convertPreOpenAcc acc 

convertPreOpenAcc :: Delayable a => PreOpenAcc OpenAcc aenv a -> EnvM S.Exp

-- The environment argument is used to convert de Bruijn indices to vars:
convertPreOpenAcc e = 
  case e of 
    Let acc1 acc2 -> 
       do (v,a2) <- withExtendedEnv "a"$ 
                    convertOpenAcc acc2 
	  a1     <- convertOpenAcc acc1
          return$ S.Let v a1 a2

--    Avar idx -> return$ S.Vr (S.var$ show$ idxToInt idx)
    Avar idx -> S.Vr <$> envLookup (idxToInt idx)

    ZipWith f acc1 acc2 -> S.ZipWith <$> convertFun f
                                     <*> convertOpenAcc acc1
                                     <*> convertOpenAcc acc2

    Let2       acc1 acc2 -> error$ "Let2"
    PairArrays acc1 acc2 -> error$ "PairArrays"
    Apply (Alam (Abody funAcc)) acc -> error$ "Apply"
    Apply _afun _acc -> error "This case is impossible"

    Acond cond acc1 acc2 -> S.Cond <$> convertExp cond 
                                   <*> convertOpenAcc acc1 
                                   <*> convertOpenAcc acc2

    -- This is real live runtime array data:
    Use arr -> return$ S.Use

    Unit e -> error "unit"
    Generate sh f -> error "generate"
    Reshape e acc -> error "reshape"
    Replicate sliceIndex slix acc -> error "replicate"
    Index sliceIndex acc slix  -> error "Index"
    Map f acc -> error "map"
    Fold     f e acc -> error "fold"
    Fold1    f   acc -> error "fold1"
    FoldSeg  f e acc1 acc2 -> error "foldseg"
    Fold1Seg f   acc1 acc2 -> error "fold1seg"
    Scanl  f e acc -> error "scanl"
    Scanl' f e acc -> error "scanl'"
    Scanl1 f   acc -> error "scanl1"
    Scanr  f e acc -> error "scanr"
    Scanr' f e acc -> error "scanr'"
    Scanr1 f   acc -> error "scanr1"
    Permute f dftAcc p acc -> error "permute"
    Backpermute e p acc -> error "backperm"
    Stencil  sten bndy acc -> error "stencil"
    Stencil2 sten bndy1 acc1 bndy2 acc2 -> error "stencil2"


--------------------------------------------------------------------------------
-- Convert Accelerate Scalar Expressions
--------------------------------------------------------------------------------
    
-- Evaluate a closed expression
convertExp :: Exp aenv t -> EnvM S.Exp
convertExp e = convertOpenExp e 

convertOpenExp :: OpenExp env aenv a -> EnvM S.Exp
convertOpenExp e = 
  case e of 
    Var idx -> S.Vr <$> envLookup (idxToInt idx)
    PrimApp p arg -> convertPrimApp p arg

    Tuple tup -> convertTuple tup
    Const c   -> error "Const" -- Sugar.toElt c
    Prj idx e -> error "Prj"
    IndexNil       -> error "IndexNil"
    IndexCons sh i -> error "IndexCons"
    IndexHead ix   -> error "IndexHead"
    IndexTail ix   -> error "IndexTail"
    IndexAny       -> error "IndexAny"

    Cond c t e  -> error "Cond"
    PrimConst c -> error "PrimConst"
    IndexScalar acc ix -> error "IndexScalar"
    Shape acc -> error "Shape"
    Size acc  -> error "Size"


-- Convert a tuple expression to our simpler Tuple representation (containing a list):
-- convertTuple :: Tuple (PreOpenExp acc env aenv) t' -> S.Exp
convertTuple :: Tuple (PreOpenExp OpenAcc env aenv) t' -> EnvM S.Exp
convertTuple NilTup = return$ S.Tuple []
convertTuple (SnocTup tup e) = 
    do e' <- convertOpenExp e
       tup' <- convertTuple tup
       case tup' of 
         S.Tuple ls -> return$ S.Tuple$ ls ++ [e']
         se -> error$ "convertTuple: expected a tuple expression, received:\n  "++ show se

convertTupleExp :: PreOpenExp OpenAcc t t1 t2 -> EnvM [S.Exp]
convertTupleExp e = do
  e' <- convertOpenExp e
  case e' of 
    S.Tuple ls -> return ls
    se -> error$ "convertTupleExp: expected a tuple expression, received:\n  "++ show se

--------------------------------------------------------------------------------
-- Convert Accelerate Primitive Applications: 
--------------------------------------------------------------------------------

convertPrimApp :: (Sugar.Elt a, Sugar.Elt b)
               => PrimFun (a -> b) -> PreOpenExp OpenAcc env aenv a
               -> EnvM S.Exp
convertPrimApp p arg = 
  do args' <- convertTupleExp arg
     return$ S.PrimApp (op p) args'
 where 
   op p = 
    case p of 
      PrimAdd ty -> error "add"
      PrimMul ty -> S.NP S.Mul
      _ -> error$ "primapp not handled yet: "++show (PrimApp p arg)

numty nt = 
  case nt of 
    IntegralNumType ty -> undefined
    FloatingNumType ty -> undefined

--------------------------------------------------------------------------------
-- Convert Accelerate Functions
--------------------------------------------------------------------------------

-- Evaluate open function
convertFun :: OpenFun env aenv t -> EnvM S.Exp
convertFun (Body b) = convertOpenExp b
convertFun (Lam f)  = fmap snd $ 
                 withExtendedEnv "v" $ do
                   v <- envLookup 0
		   S.Lam v <$> convertFun f