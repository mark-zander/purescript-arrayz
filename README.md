# purescript-arrayz

Array utilitise that have not yet made it to Purescript.

## ArrayIx

Indexing of fixed length arrays with a phantom type of a
`BoundedEnum` determining the size of the array. All
indexing into the array comes from this `BoundedEnum` type.

## ZipArray

Just like `ZipList` but since this isn't a an infinite data
structure, a `pure` function can not be easily defined.
