stack_vector
===================

This is a version of `std::vector` which strictly is allocated on the stack (no allocations required).

This includes additional specialized non-standard functions.

Additional modifications functions for faster (but unsafe manipulations):
```c
	template <class... Args> constexpr reference unchecked_emplace_back(Args &&...args) {
		iterator it = end();
		::new ((void *)it) T(::std::forward<Args>(args)...);
		_size += 1;
		return *it;
	};


	// shove_back's (unchecked_push_back)
	constexpr void shove_back(const T &value) {
		::new ((void *)end()) T(::std::forward<const T &>(value));
		_size += 1;
	}
	constexpr void shove_back(T &&value) {
		::new ((void *)end()) T(::std::forward<T &&>(value));
		_size += 1;
	}

	void append(size_type count, const T &value) {
		size_t space_remaining = capacity() - size();
		if (count && count <= space_remaining) [[likely]] {
			::stack_vector::details::uninitialized_fill_n(end(), count, value);
			_size += count;
		} else {
			if constexpr (::stack_vector::details::error_handler !=
				::stack_vector::details::error_handling::_noop) {
				return_error(false, "stack_vector cannot allocate space to insert");
			}
		}
	}
	template <typename It1> void append(It1 first, It1 last) {
		append_range(first, last);
	}
```

Usage
-----

`stack_vector` is ideal for eaither scratch work on trivially destructible data with a known maximum size or capacity.
With a slight additional cost of space it is a fantastic drop-in replacement for `std::array` where vector-like functionality is needed.
```c
stack_vector<int,1024> buffer{};
for (size_t i = 0; i < 512; i++) {
	buffer.unchecked_push_back(i);
}
```