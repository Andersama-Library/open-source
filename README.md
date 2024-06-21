Andersama's Open Source Library
===================

<img style="float: right" alt="logo" src="https://github.com/Andersama-Library/open-source/assets/25020235/7de0b5da-9377-4109-81c4-9eef31866c3f">
<!--<img alt="logo" src="https://github.com/Andersama-Library/licensed/assets/25020235/ca26c575-4cfe-4817-9a22-e0f6fe7de9d5" width="40%" style="float: right"></img>-->

There are many programming libraries out there, and here is part of my own. The utilties here are written to be:
* **Performant:** Whatever approach is taken, whether naive or complex the overall result should be fast.
* **Scalable:** The algorithms and processes chosen are ideally linear or sublinear where possible.
* **Memory Conscious:** Functions don't allocate if they don't need to.
* **Exceptionless:** Functions don't throw exceptions if they don't need to.

Notes and Usage
---------------
This branch is just a landing page, for utilities for a particular language check out the other branches.
The contents of this repo in general will be under the license provided in [LICENSE.md](LICENSE.md). 

For organizational purposes source files related to particular utilties will be under their own folders like [forceinline](/forceinline).

## C++ Single Header Files and Libraries
* [C++ Branch](https://github.com/Andersama-Library/open-source/tree/c%2B%2B) - A collection of some of my personal c++ libraries
  * [forceinline](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/forceinline) - A header for a macro to force inline functions `always_force_inline`
  * [headers](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/headers) - A set of headers that groups common standard c++ headers together
  * [simple_benchmark](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/simple_benchmark) - A single header for benchmarking, even smaller than nanobench!
  * [stack_vector](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/stack_vector) - A header for a vector which lives on the stack
  * [inline_vector](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/inline_vector) - A header to treat contiguous data like a vector in a span-like manner
  * [waterhash](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/waterhash) - An implementation of waterhash, but as a module and constexpr!
  * [wheathash](https://github.com/Andersama-Library/open-source/tree/c%2B%2B/wheathash) - An implementation of wheathash, but as a module and constexpr!
  * [and more!...](https://github.com/Andersama-Library/open-source/tree/c%2B%2B)
