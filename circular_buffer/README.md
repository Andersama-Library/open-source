circular_buffer
===============

This is an implementation of a dynamic circular buffer with support for ranges and polymorphic allocators.


Usage
-----

```c
	circular_buffer::circular_buffer<int> circles;

	circles.reserve(10);
	std::array<int, 5> nums{ 0,1,2,3,4 };
	circles.assign(nums.begin(),nums.end());

	std::cout << "Count Up:\n"
	for (int n : circles) {
		std::cout << n << '\n';
	}

	// continuing from the last written value
	// to make a saw tooth pattern
	for (size_t i = 0; i < 5; i++) {
		circles.overwrite_back(5-i);
	}
	for (int n : circles) {
		std::cout << n << '\n';
	}

	std::cout << "\nSawTooth:\n";
	// continuing...we might've wrapped around at this point*
	// but now we're doing we'll keep going with another
	// monotonic sequence
	for (size_t i = 0; i < 10; i++) {
		circles.overwrite_back(i);
	}
	for (int i = 0; i < circles.size(); i++) {
		std::cout << circles[i] << '\n';
	}

	// now for some fun
	// overwrite_write_index() ignores incrmenting the internal read index
	// allowing the write index to keep spinning away, increasing size ABOVE
	// capacity! HEY, but []'s will still wrap around!

	for (size_t i = 0; i < 10; i++) {
		circles.overwrite_write_index(i);
	}
	
	// now size() is 20~ where capacity() is still 10!
	// with this trick you can compress repeated sequences into the circular_buffer!
	for (size_t i = 0; i < circles.size(); i++) {
		std::cout << circles[i] << '\n';
	}

	// we can use the circular like a vector if we'd like
	// that's no problem! emplace_back will expand the circular buffer
	// as needed just like a vector
	for (size_t i = 0; i < 10; i++) {
		circles.emplace_back(i);
	}
```