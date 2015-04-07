This talk describes my project at Tabula Inc for compiling Haskell to hardware.

I gave an earlier version of this talk first at IFIP Working Group 2.8 (functional programming) in 2014 and a [newer version](https://galois.com/blog/2015/04/tech-talk-haskell-hardware-via-cccs/) at Galois in Portland in April 2015.

Abstract:

 <blockquote>

For the last several years, speed improvements in computing come mainly from increasing parallelism. Imperative programming, however, makes parallelization very difficult due to the many possible dependencies implied by effects. For decades, pure functional programming has held the promise of parallel execution while retaining the very simple semantics that enables practical, rigorous reasoning. This talk describes a prototype compiler from Haskell (not a library) to low-level hardware descriptions for massively parallel execution on reprogrammable logic devices. The compiler works by monomorphizing, miscellaneous other transformations, and conversion to the vocabulary of cartesian closed categories (CCCs), as captured in a small collection of Haskell type classes. One instance of those classes provides an interpretation as parallel circuits. I will show many examples of simple Haskell programs and corresponding compiler-generated circuits.

 </blockquote>
