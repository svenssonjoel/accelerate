{-# LANGUAGE GADTs, RankNTypes #-} 
{-# LANGUAGE FlexibleInstances, PatternGuards, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}


------------------------------------------------------------------------------
{- Thougths on Accelerate -> ArBB Bindings
 
   CUDA Bindings generates the program in "stages" that
   correspond to the "kernel"-Skeletons. 
   It may be the case that the ArBB backend can adopt a more 
   unstaged approach. The entire Accelerate program could 
   perhaps be compiled into a single ArBB function. 
   
    + Intermediate arrays managed entirely by ArBB. 
    
    - Only possible if all the Accelerate concepts are 
      implementable entirely within ArBB (That we wont
      to emulate any accelerate functionality) 

-- About Accelerate

   
   CUDA.hs contains a function called run :: Arrays a => Acc a -> a 
      ArBB backend needs to export the same. 
   AST.hs contains the typeclass Arrays.      



-}



module Data.Array.Accelerate.ArBB where 

import Intel.ArbbVM
import Intel.ArbbVM.Convenience

import Data.Array.Accelerate.AST
import Data.Array.Accelerate.Tuple
import qualified Data.Array.Accelerate.Type as Type
import Data.Array.Accelerate.Array.Sugar  (Array(..), Segments, eltType)
import qualified Data.Array.Accelerate.Array.Sugar as Sugar
import qualified Data.Array.Accelerate.Array.Data as AD 

import Data.Array.Accelerate.Array.Representation

import Data.Array.Accelerate.Analysis.Type

import Foreign.Ptr 
import Foreign.Marshal.Array


import Data.Array.Accelerate.ArBB.Data
import Data.Array.Accelerate.ArBB.Type

import Data.Typeable
import Data.Int
import Data.IORef

import qualified Data.Map as M

import System.IO.Unsafe


type ArBBEnv = [Variable]    
    
------------------------------------------------------------------------------
-- run (The entry point)
run :: Arrays a => Acc a -> a 
run acc = unsafePerformIO$ arbbSession$ do 
    
    undefined 
     

{- 
   tip: 

   Use bind_to_host to map an array created using arbb_op_alloc 
   into host memory and then copy data into the "host" side pointer
   This method should have performance benefits if "kernels" are 
   reused. However, I am unsure the method will affect our performance 
   any. It may, though, be a method that works right now, since 
   the copy-in scenario seems buggy.

   Using bind_to_host should still be compatible with the approach 
   I have started but requires "more work" because our code needs
   to perform the actuall copying (instead of the ArBB system taking 
   care of it automatically) 
   

   Thing to consider: 
     The is_remote argument to functions. what does it mean ? 
     
   

-} 

executeArBB :: (Typeable aenv, Typeable a) => OpenAcc aenv a -> EmitArbb ()
executeArBB acc = do
    let gb = collectGlobals acc (M.empty)
    glob_vars <- bindGlobals gb 
    let lst = M.toList glob_vars
        my_v = snd (head lst)   

    dummy <- getScalarType_ ArbbI32 -- cheat
    dt    <- getDenseType_ dummy 1  -- cheat
 

    -- An ArBB function with no inputs. (Ok ? ) 
    fun <- funDef_ "main" [dt] [] $ \ o [] -> do 
       o1 <- executeArBB' acc glob_vars
       assignTo o o1 
 
---------
    str <- serializeFunction_ fun 
    liftIO$ putStrLn (getCString str)
---------  

--- CHEAT    
   
    withArray_ (replicate 1024 0 :: [Int32]) $ \ out -> do 
      outb <- createDenseBinding_ (castPtr out) 1 [1024] [4]
      gout <- createGlobal_ dt "output" outb  
      vout <- variableFromGlobal_ gout 
      
      execute_ fun [vout] [my_v] -- [vin]
 
  
      result <- liftIO$ peekArray 1024 out
      liftIO$ putStrLn (show result)
      return ()
--------    
    return ()
    

-- TODO: Do I need the Typeable ? 
executeArBB' :: (Typeable aenv, Typeable a) => 
                OpenAcc aenv a -> 
                GlobalBindings Variable -> 
                EmitArbb [Variable] 
executeArBB' acc@(OpenAcc pacc) gv = 
  case pacc of
     (Use (Array sh ad)) -> return (lookupArray ad gv) 
     m@(Map f acc) -> execMap (getAccType (OpenAcc m))  -- output type (of elements)
                              (getAccType  acc)         -- input type (of elemets)  
                              f =<< executeArBB' acc gv 

        
execMap ot it f inputs = do 
  fun <- genMap ot -- output type (of elements)
                it -- input type (of elemets)  
                f 
  
  out_dense <- defineDenseTypes ot
  inp_dense <- defineDenseTypes it
  out_vars  <- defineLocalVars out_dense
  inp_vars  <- defineLocalVars inp_dense

  assignTo inp_vars inputs
  map_ fun out_vars inp_vars     
  return out_vars
  

------------------------------------------------------------------------------
-- What to do in case of Map 
genMap :: [ScalarType] -> [ScalarType] -> OpenFun env aenv t -> EmitArbb Function 
genMap out inp fun = do
  out' <- defineTypes out
  inp' <- defineTypes inp
  -- Start by generating the function to be mapped!
  fun <- funDef_ "f" out' inp' $ \ outs inps -> do 
    vars <- genFun fun inps -- inputs as the "environment"  
    assignTo outs vars
----------
  str <- serializeFunction_ fun 
  liftIO$ putStrLn "mapee function" 
  liftIO$ putStrLn (getCString str)
---------  
    
  return fun
 
------------------------------------------------------------------------------
-- Assign outputs of something to a list of variables 
assignTo [] [] = return () 
assignTo (x:xs) (y:ys) = do 
   op_ ArbbOpCopy [x] [y] 
assignTo _ _ = error "AssignTo: Mismatch!"

------------------------------------------------------------------------------
-- define ArBB VM types for a list of type "names" 
defineTypes :: [ScalarType] -> EmitArbb [Type]
defineTypes [] = return []
defineTypes (x:xs) = do 
   t <- getScalarType_ x 
   ts <- defineTypes xs 
   return (t:ts)

defineDenseTypes :: [ScalarType] -> EmitArbb [Type] 
defineDenseTypes [] = return []
defineDenseTypes (x:xs) = do 
   t <- getScalarType_ x
   d <- getDenseType_ t 1 
   ds <- defineDenseTypes xs 
   return (d:ds)
 

defineLocalVars :: [Type] -> EmitArbb [Variable]
defineLocalVars [] = return [] 
defineLocalVars (t:ts) = do 
  let name = "name"
  liftIO$ putStrLn ("Creating local variable: " ++name)    
  v <- createLocal_ t name -- "name" -- name needs to be unique ? 
  vs <- defineLocalVars ts
  return (v:vs) 

------------------------------------------------------------------------------
-- generate code for function 
-- Bunch of Lambdas followed by a Body 
-- The body uses "De Bruijn" indices (so no variable name binding in lam)  
genFun :: OpenFun env aenv t -> ArBBEnv -> EmitArbb [Variable] 
genFun (Lam lam)   = genFun lam 
genFun (Body body) = genExp body 

------------------------------------------------------------------------------
-- genExp 
-- input expression.
-- output Arbb variables holding valuation of expression

-- TODO: Some Expressions contain arrays (IndexScalar) 
--       The Accelerate guys uses the "liftAcc" machinery
--       in the Execute module to address this issue. 

genExp :: forall env aenv t. 
          OpenExp env aenv t -> ArBBEnv -> EmitArbb [Variable]
genExp (Const c) _ = genConst (eltType (undefined::t)) c 
genExp app@(PrimApp f arg) env = do 
   res <- genPrimApp f arg (head (getExpType app)) env
   return [res] 
genExp (Tuple t) env = genTuple t env
genExp (Var idx) env = return [env !! idxToInt idx] 
--genExp s env = do liftIO$ putStrLn (show s); return [] 

genConst :: Type.TupleType a -> a -> EmitArbb [Variable]
genConst Type.UnitTuple  _       = return [] 
genConst (Type.SingleTuple ty) c = do
  s <- arbbConst ty c 
  return [s]
genConst (Type.PairTuple ty1 ty0) (cs,c) = do
  s1 <- genConst ty1 cs
  s2 <- genConst ty0 c 
  return (s1 ++ s2)  

------------------------------------------------------------------------------
-- genPrimApp
-- Is it possible to have a "multi-scalar" result from a primApp? 
--   (Complex number?) 
--   I'm going with that the answer is no. 
-- 
-- Inputs: 
--  Function
--  Inputs
--  Type of output 
--  Environment       
genPrimApp :: PrimFun c -> 
              OpenExp env aenv t -> 
              ScalarType -> 
              ArBBEnv -> 
              EmitArbb Variable
genPrimApp op args st env = do 
   inputs <- genExp args env
   sty <- getScalarType_ st 
   let resname = "res" -- needs a unique name? 
   liftIO$ putStrLn ("Creating a result variable: " ++resname)
   res <- createLocal_ sty resname
   genPrim op res inputs
   return res    

------------------------------------------------------------------------------
-- Primitive operations (ADD, SUB, MUL etc) 
genPrim :: PrimFun c -> Variable -> [Variable] -> EmitArbb Variable
genPrim (PrimAdd _) out inputs = do 
   op_ ArbbOpAdd [out] inputs
   return out 
genPrim (PrimSub _) out inputs = do 
   op_ ArbbOpSub [out] inputs
   return out 
genPrim (PrimMul _) out inputs = do 
   op_ ArbbOpMul [out] inputs 
   return out


------------------------------------------------------------------------------
-- Tuple Expression! 
genTuple :: Tuple (OpenExp env aenv) t -> ArBBEnv -> EmitArbb [Variable] 
genTuple NilTup _ = return [] 
genTuple (SnocTup tup e) env = do 
   vars <- genExp e env 
   rest <- genTuple tup env 
   return (rest ++ vars)

------------------------------------------------------------------------------
-- More type machinery ! 
arbbConst :: Type.ScalarType a -> a -> EmitArbb Variable
arbbConst t@(Type.NumScalarType (Type.IntegralNumType ty)) val
 | Type.IntegralDict <- Type.integralDict ty  -- What is this syntax ??  
  = int32_ val
arbbConst t@(Type.NumScalarType (Type.FloatingNumType (Type.TypeFloat _))) val
  = float32_ val
arbbConst t@(Type.NumScalarType (Type.FloatingNumType (Type.TypeDouble _))) val
  = float64_ val
-- TODO: Keep going for all Accelerate Types




------------------------------------------------------------------------------
-- idxToInt -- This is defined in one of the CUDA backend files 
idxToInt :: Idx env t -> Int
idxToInt ZeroIdx       = 0
idxToInt (SuccIdx idx) = 1 + idxToInt idx


