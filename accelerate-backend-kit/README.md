accelerate-backend-kit
=======================================================

`Data.Array.Accelerate.SimpleAST` defines a basic Haskell 98
representation of Accelerate abstract syntax trees.  This simplifiied
AST can be useful for those trying to learn the underlying model or
for those interested in creating a new backend for Accelerate.

`Data.Array.Accelerate.SimpleConverter` converts expressions of type
`Acc` into the simple representation.

Note that if you are using this package for building a new backend, it
is still recommended to do any AST to AST transformations (compiler
passes) using the more strongly typed internal Accelerate AST.  This
will be more work, but will increase assurances that your program
transformations do not introduce bugs.


WARNING -- THIS PACKAGE IS CURRENTLY UNFINISHED [2012.04.13]
------------------------------------------------------------

In particular `use` constructs are not supported yet.  This is an
alpha-stage prototype.

