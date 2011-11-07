module Data.Array.Accelerate.SimpAST () where

  import qualified Data.Map as Map -- importing Map for an environment
  
  -- I'd like to use their array, dimension/shape,
  -- slicing/index representations, and element, but it might make the
  -- whole AST more complex.
  import Data.Array.Accelerate.Array.Representation
  import Data.Array.Accelerate.Array.Sugar
    
  -- The idea of this module is to get rid of alll the fancy type
  -- trickery, and turn it into a pretty dumb AST, that could be parsed
  -- via recursive decent. You shouldn't need all the fancy types, as
  -- we know the complex AST type checks. The goal is to simplify the
  -- backend creation by compiling away all that type information.

  -- A simple representation of variables
  -- Honestly though, since we're trying to convert from de Brujin
  -- indicies to this... it might just as well use the indicies. 
  --
  data Var = Var String

  -- A simple environment
  -- {Var -> Expression}
  type Env = Map Var AccExp

  -- Arrays and Shapes
  --
  -- So it looks like, while the array is very abstract, the EltRep sh
  -- (the dimension) is pretty much always going to be what amount to a
  -- list of ints. Of course, they create their own data structure which
  -- amounts to a list of ints... I think I'll just use a list of ints.
  -- I suppose I could use their structure, but I don't see a reason to
  -- yet, other than one less thing I'll have to implement. However, if
  -- I do, I think I'll have to deal with a ton of crazy type class
  -- stuff. I'm also ignoring the fact that it's much more abstract than
  -- that, but I honestly don't see what all that abstraction buys us.
  --
  -- The problem is the ArrayData and ArrayElt have type class
  -- methods, and numerous instances, and I can't imagine they're all
  -- useless. But, I can't quite understand everything in Array.Data
  -- yet.
  --
  -- The Shape type class also has some methods that I'm ignoring.
  --
  -- However, the real problem here is the array's need to be passed out
  -- to foreign code and back into Haskell. The Data module handles all
  -- of this... so it would be best to reuse it.
  dimx :: Int -> [Int]
  dimx 0 = []
  dimx x = [1 .. x]

  type Dimension = [Int]
  type DIM0 = []
  type DIM1 = dimx 1
  type DIM2 = dimx 2
  type DIM3 = dimx 3
  type DIM4 = dimx 4
  type DIM5 = dimx 5
  type DIM6 = dimx 6
  type DIM7 = dimx 7

  type Scalar e = Array DIM0 e
  type Vector e = Array DIM1 e
  type Segments = Vector Int

  data Array e = Array [Int]
  
  -- A simpler, but not quite simple, AST. Necessary I think, as they
  -- make (too) heavy use of type classes and other type ninjary.
  data AccExp where
    Let :: (Arrays array, Arrays body) => Var -> array -> body -> AccExp
      -- Let binds an array to some var in a body
      -- Let var array body
    Let :: (Arrays array1, Arrays array2, Arrays Body) =>
      (Var, Var) -> (array1, array2) -> body -> AccExp
    PairArrays :: 


  -- A simple AST
  -- I wish I could make it this simple, but I fear not.
  data AccExp = 
      Int
      -- The element types. Only Int for now, but the others would be
      -- easy enough to add, I think.
    | Array ??? AccExp
      -- Array Dimension Element
    | Unit AccExp
      -- Unit Element -- Turn an element into a singleton array
    | Let Var AccExp AccExp 
      -- Let Binder Bindee Body -- Bind the array in the var. Use for
      -- common subexpression elimination
    | LetPair (Var, Var) AccExp AccExp 
      -- Let (Var1, Var2) (PairArrays Array1 Array2) Body
    | PairArrays AccExp AccExp
      -- PairArrays Array1 Array2
    | Var
      -- Var "x"
    | Apply AccExp AccExp
      -- Function $ Argument
    | Cond Bool AccExp AccExp 
      -- If Bool Con Alt
    | Use AccExp
      -- Use Array
    | Reshape ??? AccExp 
      -- Reshape Shape Array
    | Generate AccExp AccExp
      -- Generate Function Array, very similar to map
    | Replicate ??? ??? AccExp
      -- Replicate IndexOfSomeKind? SomeValue Array
    | Index ??? AccExp ???
      -- Index SomeMultiDimensionalIndex Array 'SliceValue'?
    | Map AccExp AccExp
      -- Map Function Array
    | ZipWith AccExp AccExp AccExp
      -- ZipWith Function Array1 Array2
    | Fold AccExp AccExp AccExp
      -- Fold Function Default Array
    | Fold1 AccExp AccExp
      -- Fold1 Function Array
    | FoldSeg AccExp AccExp AccExp ???
      -- FoldSeg Function Default Array 'Segment Descriptor'
    | Fold1Seg AccExp AccExp ???
      -- FoldSeg Function Array 'Segment Descriptor'
    | Scanl AccExp AccExp AccExp
      -- Scanl Function InitialValue LinearArray
    | Scanl' AccExp AccExp AccExp
      -- Scanl' Function InitialValue LinearArray
    | Scanl1 AccExp AccExp
      -- Scanl Function LinearArray
    | Scanr AccExp AccExp AccExp
      -- Scanr Function InitialValue LinearArray
    | Scanr' AccExp AccExp AccExp
      -- Scanr' Function InitialValue LinearArray
    | Scanr1 AccExp AccExp
      -- Scanr Function LinearArray
    | Permute AccExp AccExp AccExp AccExp
      -- Permute Function DefaultArray PermuteFunction
      -- SourceArray
    | Backpermute ??? AccExp AccExp
      -- Backpermute DimensionsOfReulst PermuteFunction
      -- SourceArray
    | Stencil AccExp ??? AccExp
      -- Stencil Function BoundaryCondition SourceArray
    | Stencil2 AccExp ??? AccExp ??? AccExp
      -- Stencial2 Function Boundary1 Array1 Boundary2 Array2
    | Lam Var AccExp
      -- \Var -> Body
--    | Prim
      -- Any of the primitive functions

