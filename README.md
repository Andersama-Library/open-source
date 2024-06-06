Andersama's Open Source Library
===================

<img style="float: right" alt="logo" src="https://github.com/Andersama/Andersama/assets/25020235/9a783e1b-9039-4b4e-b9e4-073d3344102a">
<!--<img alt="logo" src="https://github.com/Andersama-Library/licensed/assets/25020235/ca26c575-4cfe-4817-9a22-e0f6fe7de9d5" width="40%" style="float: right"></img>-->

There are many programming libraries out there, and here is part of my own. The utilties here are written to be:
* **Performant:** Whatever approach is taken, whether naive or complex the overall result should be fast.
* **Scalable:** The algorithms and processes chosen are ideally linear or sublinear where possible.
* **Memory Conscious:** Functions don't allocate if they don't need to.
* **Exceptionless:** Functions don't throw exceptions if they don't need to.

Notes and Usage
---------------
This branch is a collection of c++ libraries, for utilities for a particular language check out the other branches.
The contents of this repo in general will be under the license provided in [LICENSE.md](LICENSE.md). The licenses of some folders may be different be sure to check.

CMake Single Headers
---------------
```cmake
include(FetchContent)

FetchContent_Declare(
    open_source
    GIT_REPOSITORY https://github.com/Andersama-Library/open-source.git
    GIT_BRANCH c++
)

FetchContent_MakeAvailable(open_source)
include_directories(${open_source_SOURCE_DIR}/)
# we should now be able to #include "forceinline/forceinline.h"
```

For organizational purposes source files related to particular utilties will be under their own folders like [forceinline](/forceinline).
