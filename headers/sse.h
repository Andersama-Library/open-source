#pragma once

#if defined(__clang__) || defined(__GNUC__) && (defined(__x86_64__) || defined(__i386__))
#include <x86intrin.h>
// we do this check afterwards because
// MSVC clang support defines both __clang__ and _MSC_VER
#elif defined(_MSC_VER) && (_MSC_VER >= 1800)
// MSVC's intrin.h includes immintrin.h
#include <intrin.h>
#else
//#include <xmmintrin.h>
//#include <emmintrin.h>
//#include <pmmintrin.h>
//#include <tmmintrin.h>
//#include <smmintrin.h>
//#include <nmmintrin.h>
//#include <ammintrin.h>
//#include <zmmintrin.h>

//see: https://github.com/rstudio/libclang-builtin-headers/blob/master/builtin-headers/3.5/immintrin.h
//see: https://github.com/llvm/llvm-project/blob/main/clang/lib/Headers/immintrin.h
#include <immintrin.h>
#endif
