inline_vector
=============

This is a `std::span` reference like structure for `std::vector` like data types, it uses a similar structure of 3 pointers to mark the vector's contents.

This includes additional specialized non-standard functions:

For reserving typically after construction:
```c
	// DANGER
	[[nodiscard]] size_t unchecked_reserve(size_type n)
	{
		size_t c = _cap - _data;
		size_t m = std::max(c, n);
		_cap     = _data + m;
		return m;
	}
```

and additional modifications functions for faster (but unsafe manipulations):
```c
	template<class... Args> constexpr reference unchecked_emplace_back(Args &&...args)
	{
		iterator it = end();
		::new ((void *)it) T(::std::forward<Args>(args)...);
		_end += 1;
		return *it;
	};

	// shove_back's (unchecked_push_back)
	constexpr void shove_back(const T &value)
	{
		unchecked_emplace_back(value);
	}

	constexpr void shove_back(T &&value)
	{
		unchecked_emplace_back(value);
	}

	// append's (non-standard)
	void append(size_type count, const T &value)
	{
		size_t space_remaining = capacity() - size();
		if (count && count <= space_remaining) [[likely]] {
			::inline_vector::details::uninitialized_fill_n(end(), count, value);
			_end += count;
		} else {
			if constexpr (::inline_vector::details::error_handler !=
							::inline_vector::details::error_handling::_noop) {
				return_error(false, "inline_vector cannot allocate space to insert");
			}
		}
	}

	template<typename It1> void append(It1 first, It1 last)
	{
		append_range(first, last);
	}
```

Usage
-----

`inline_vector` is ideal for eaither scratch work on trivially destructible data, as it
allows the use of non-standard functions to transform another vector's contents.

```c
//... work with std::vector
std::vector<int> owning_vector{};
owning_vector.reserve(1024);
for (size_t i = 0; i < 512; i++) {
	owning_vector.push_back(i);
}

//... work with the inline_vector using the std::vector
inline_vector<int> nonowning_vector{owning_vector.data(),owning_vector.data()+owning_vector.size(),owning_vector.data()+owning_vector.capacity());
//or
//inline<vector<int> nonowning_vector{owning_vector};

for (size_t i = 0; i < 512; i++) {
	nonowning_vector.unchecked_push_back(i);
}
//... and on

//... finally eventually collapse the manipulations in the inline_vector into std::vector
owning_vector.assign(nonowning_vector.data(), nonowning_vector.data()+nonowning_vector.size());

```

Alternatively, `inline_vector` can be used to treat already existing contiguous data in a vector-like manner.
```c
std::array<int, 1024> stack_buffer{};

inline_vector<int> vector_like{stack_buffer.data(),stack_buffer.data(),stack_buffer.data()+stack_buffer.size());
//or
//inline_vector<int> vector_like{stack_buffer};
for (size_t i = 0; i < 1024; i++) {
	vector_like.unchecked_push_back(i);
}
```

Another usage is the passing of vector-like types via values manner like a `std::span`, without introducing a copy.