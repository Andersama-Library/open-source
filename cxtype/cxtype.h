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
#include <array>

namespace cxtype {
	constexpr char alpha_mask = 0x1;
	constexpr char punct_mask = 0x2;
	constexpr char underscore_mask = 0x4;
	constexpr char decimal_mask = 0x8;
	
	constexpr char hexidecimal_mask = 0x10;
	constexpr char control_mask = 0x20;
	constexpr char whitespace_mask = 0x40;
	constexpr char blank_mask = 0x80;
	
	constexpr std::array<char, 256> char_details = []() {
		std::array<char, 256> t = {};
		for (size_t i = 'a'; i <= 'z'; i++)
			t[i] |= alpha_mask;//lowercase_mask;
		for (size_t i = 'A'; i <= 'Z'; i++)
			t[i] |= alpha_mask;//uppercase_mask;
		t['_'] |= underscore_mask;
		for (size_t i = '0'; i <= '9'; i++) {
			t[i] |= decimal_mask | hexidecimal_mask;
		}

		for (size_t i = 'a'; i <= 'f'; i++)
			t[i] |= hexidecimal_mask;
		for (size_t i = 'A'; i <= 'F'; i++)
			t[i] |= hexidecimal_mask;

		for (size_t i = 0; i < 32; i++)
			t[i] |= control_mask;

		t[127] |= control_mask;

		t['\t'] |= whitespace_mask | blank_mask;

		t['\n'] |= whitespace_mask;
		t['\v'] |= whitespace_mask;
		t['\f'] |= whitespace_mask;
		t['\r'] |= whitespace_mask;

		t[' '] |= whitespace_mask | blank_mask;

		for (size_t i = '!'; i <= '/'; i++)
			t[i] |= punct_mask;

		for (size_t i = ':'; i <= '@'; i++)
			t[i] |= punct_mask;

		for (size_t i = '['; i <= '`'; i++)
			t[i] |= punct_mask;

		for (size_t i = '{'; i <= '~'; i++)
			t[i] |= punct_mask;

		return t;
	}();

	constexpr bool isalnum(unsigned char c) noexcept {
		return cxtype::char_details[c] & (alpha_mask | decimal_mask);
	}

	constexpr bool isalpha(unsigned char c) noexcept {
		return cxtype::char_details[c] & alpha_mask;
	}

	constexpr bool islower(unsigned char c) noexcept {
		return ((unsigned char)(c - 'a')) < 26;
	}

	constexpr bool isupper(unsigned char c) noexcept {
		return ((unsigned char)(c - 'A')) < 26;
	}

	constexpr bool isdigit(unsigned char c) noexcept {
		return ((unsigned char)(c - '0')) < 10;
	}

	constexpr bool isxdigit(unsigned char c) noexcept {
		return cxtype::char_details[c] & hexidecimal_mask;
	}

	constexpr bool isodigit(unsigned char c) noexcept {
		return ((unsigned char)(c - '0')) < 8;
	}

	constexpr bool iscntrl(unsigned char c) noexcept {
		return cxtype::char_details[c] & control_mask;
	}

	constexpr bool isgraph(unsigned char c) noexcept {
		return ((unsigned char)(c - 33)) < 94;
	}

	constexpr bool isprint(unsigned char c) noexcept {
		return ((unsigned char)(c - 32)) < 95;
	}

	constexpr bool isspace(unsigned char c) noexcept {
		return cxtype::char_details[c] & whitespace_mask;
	}

	constexpr bool isblank(unsigned char c) noexcept {
		return cxtype::char_details[c] & blank_mask;
	}

	constexpr bool ispunct(unsigned char c) noexcept {
		return cxtype::char_details[c] & punct_mask;
	}
}
