% Writing Multi-Core and GPU Programs using Haskell
% Mathias Körner, mkoerner@gmail.com
% HaskellerZ, 31. July 2014

Introduction
------------

  - Introduction to 2 libraries for parallel computing:

    - [Repa](https://github.com/DDCSF/repa) for matrix/array based multi-core operations.
    - [Accelerate](https://github.com/AccelerateHS/accelerate) for GPU computations.

  - Good introduction to Repa and Accelerate can be found in ["Parallel and Concurrent Programming in Haskell"](http://chimera.labs.oreilly.com/books/1230000000929/index.html) 
    by Simon Marlow.

  - If you have questions, ask!

  - Ok?


Setup
-----

  - Strong recommendation: use `ghc-7.6.3` to experiment with Repa and Accelerate.
  - Therefore samples will be run on AWS GPU (g2.2xlarge) instance 
    (8 CPUs, NVIDIA GPU with 1536 CUDA cores).
    - Does not come with Haskell support out of the box, but NVIDIA CUDA development 
      tools are installed out of the box.
  - Installation steps at [https://gist.github.com/mkoerner/189d446554a89196216a](https://gist.github.com/mkoerner/189d446554a89196216a)
  - My laptop is unfortunately far too old to work with the `accelerate-cuda` backend. 
  - With `ghc-7.8.2` the installation involved some manual intervention.

Next
----

[Repa](repa/RepaIntro.html)
