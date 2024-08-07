#pragma once
/*
MIT License

Copyright (c) 2024 Andersama-Library https://github.com/Andersama-Library/open-source

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
Usage: modifier in front of function definition or statement
*/

#ifndef always_flatten_calls
#if __GNUC__
#define always_flatten_calls [[gnu::noinline]]
#elif __clang__
#define always_flatten_calls [[clang::noinline]]
#elif _MSC_VER
#define always_flatten_calls [[msvc::noinline]]
#elif __EMSCRIPTEN__
#define always_flatten_calls
#elif __MINGW32__
#define always_flatten_calls
#elif __MINGW64__
#define always_flatten_calls
#else
#define always_flatten_calls
#endif
#endif
