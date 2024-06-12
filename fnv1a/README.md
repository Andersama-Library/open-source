fnv1a
=====

A constexpr implmentation of the fnv1a hash for 32 and 64 bit keys.

Implementation
--------------
```c
#pragma once
#include <cstdint>
 
constexpr size_t fnv1a(const uint8_t* ptr, size_t count) noexcept {
	size_t hash = 0xcbf29ce484222325ull;

	constexpr size_t fnv_prime = 0x00000100000001B3ull;

	for (size_t i = 0; i < count; i++) {
		hash = (ptr[i] ^ hash) * fnv_prime;
	}

	return hash;
}

constexpr uint32_t fnv1a_32(const uint8_t* ptr, size_t count) noexcept {
	uint32_t hash = 0x811c9dc5;

	constexpr uint32_t fnv_prime = 0x01000193;

	for (size_t i = 0; i < count; i++) {
		hash = (ptr[i] ^ hash) * fnv_prime;
	}

	return hash;
}
```

Usage
-----

```c
#include "fnv1a/fnv1a.h"
#include <vector>
#include <iostream>

int main(void) {
	std::vector<char> bytes = {'h','e','l','l','o'};

	// for a 64bit hash
	size_t hash = fnv1a((uint8_t*)bytes.data(),bytes.size());
	std::cout << hash << '\n'

	// for a 32bit hash
	size_t hash32 = fnv1a_32((uint8_t*)bytes.data(),bytes.size());
	std::cout << hash32 << '\n';

	return 0;
}
```