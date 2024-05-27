#pragma once
#include <string_view>

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

struct arguments {
	static constexpr size_t strlen(const char* arg) {
		const char* last = arg;
		while (last && *last)
			++last;
		return (size_t)(last-arg);
	}

	int argc = {};
	int contiguous_argc = {};
	const char** argv = {};
	size_t last_argument_size = {};

	constexpr arguments(int arg_count, const char* arg_values[]) {
		argc = arg_count;
		argv = arg_values;

		for (int i = 0; i < argc; i++) {
			last_argument_size = arguments::strlen(argv[i]);
			if (argv[i + 1] == 0)
				break;
			size_t constant_length = (argv[i + 1] - argv[i]) - 1;
			if (last_argument_size != constant_length)
				break;
			contiguous_argc += 1;
		}
	}

	constexpr std::string_view argument(size_t i) {
		size_t constant_length = (argv[i + 1] - argv[i]) - 1;
		return
		(i <= contiguous_argc) ? std::string_view{argv[i], (i == contiguous_argc) ? last_argument_size : constant_length} :
		(i < argc) ? std::string_view{argv[i]} : std::string_view{};
	}

	constexpr std::string_view operator[](size_t i) {
		return argument(i);
	}
};