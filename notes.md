
[*From Haskell to hardware via cartesian closed categories*]: http://conal.net/blog/posts/haskell-to-hardware-via-cccs/ "blog post"

[*Overloading lambda*]: http://conal.net/blog/posts/overloading-lambda "blog post"

[*Optimizing CCCs*]: http://conal.net/blog/posts/optimizing-cccs "blog post"

# Haskell to Hardware via CCC

## Tabula

*   Post-FPGA reconfigurable hardware.
*   Spacetime architecture:
    *   3D for shorter paths
    *   Implemented by rapid reconfiguration (2GHz)
    *   Minkowski spacetime (special relativity)
    *   Layout plus scheduling becomes just layout, satisfying causality constraints
    *   Very high sustained throughput
*   Tremendous flexibility for moving computations in space & time
*   Program in a non-sequential language: Haskell

## Overall plan

*   Use GHC to convert full Haskell to small Core language
*   Convert to more abstract representation
*   Interpret as circuits
*   Translate to hardware description language
*   Synthesize & optimize with existing machinery

## GHC Core

Show source code or excerpt from papers

## Overloading lambda

## (Bi-)Cartesian closed categories

From category theory:

*   *Category*: identity and composition
*   *Cartesian*: products (intro & elim)
*   *Closed*: exponentials (arrows as "values")
*   *Co-cartisian*: coproducts ("sums")

## Category

## Converting to CCC form

## Circuit CCC

## Examples

## Status & future
