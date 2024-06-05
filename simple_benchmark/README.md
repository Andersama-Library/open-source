simple_benchmark
================

An incredibly small benchmarking header, inspired by [nanobench](https://github.com/martinus/nanobench).
This header however fits in less than 300 lines! A truly small benchmarking library.

(NOTE: we don't include all the performance counter tracking or pretty printing that nanobench does)

Usage
-----

```c
#include <iostream>
#include <algorithm>
#include "simple_benchmark/simple_benchmark.h"
#include "stack_vector/stack_vector.h"

int main()
{
	stack_vector::stack_vector<simple_benchmark::result<1>, 3> results = {
		// we can run a test and specify a batch count (here we have 1)
		simple_benchmark::run("iostream (endl)", 1, []() {
			std::cout << "Hello CMake." << std::endl;
		}),

		// here we run another test, but omit the batch count (assumed to be 1)
		simple_benchmark::run("iostream (\\n)", []() {
			std::cout << "Hello CMake." << '\n';
		}),

		simple_benchmark::run("iostream (\\n) in string", 1, []() {
			std::cout << "Hello CMake.\n";
		}) 
	};

	std::sort(results.begin(), results.end(), [](const simple_benchmark::result<1>& lhs, const simple_benchmark::result<1>& rhs) {
		// we use the convienience function here because each test
		// accumulates a total amount of time, so we need to account for the loop and batch count of each test
		return lhs.get_average_operation_ns(0) < rhs.get_average_operation_ns(0);
		});
	
	// print the results in order of quickest to slowest (lower time taken is better)
	for (auto& result : results) {
		std::cout << result.title << "\n\t" << result.get_average_operation_ns(0) << '\n';
	}

	return 0;
}
```

Use the overloads with `allocate_tag` to use strings in place of string_views. If you need to, you can call the underlying implementation function to use a result type with a custom string type.

And some example output from the program above:
```txt
Hello CMake.
Hello CMake.
Hello CMake.
Hello CMake.
Hello CMake.
Hello CMake.
iostream (\n) in string
        46050
iostream (endl)
        112600
iostream (\n)
        149350
```