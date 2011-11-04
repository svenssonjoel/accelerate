module Data.Array.Accelerate.SimpAST () where

  import qualified Data.Map as Map
    
  -- The idea of this module is to get rid of alll the fancy type
  -- trickery, and turn it into a pretty dumb AST, that could be parsed
  -- via recursive decent. You shouldn't need all the fancy types, as
  -- we know the complex AST type checks. The goal is to simpify the
  -- backend creation by compiling away all that type information.

  -- A simple representation of variables
  --
  data Var = Var String

  -- A simple environment
  type Env = Map Var AccExp

  
  -- A simple AST
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

