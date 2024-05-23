simple_benchmark
================

This is a shameless rip of the bits that appear to make [nanobench](https://github.com/martinus/nanobench) work.
All of which fits in less than 300 lines! 

(NOTE: we don't include all the performance counter tracking or pretty printing that nanobench does)

Usage
-----

Here's an example
```c
#include <iostream>
#include <algorithm>
#include "simple_benchmark/simple_benchmark.h"
#include "stack_vector/stack_vector.h"

int main()
{
	stack_vector::stack_vector<simple_bench::batch_result<1>, 3> results = {
		// we can run a test and specify a batch count (here we have 1)
		simple_bench::run("iostream (endl)", 1, []() {
			std::cout << "Hello CMake." << std::endl;
		}),

		// here we run another test, but omit the batch count (assumed to be 1)
		simple_bench::run("iostream (\\n)", []() {
			std::cout << "Hello CMake." << '\n';
		}),

		simple_bench::run("iostream (\\n) in string", 1, []() {
			std::cout << "Hello CMake.\n";
		}) 
	};

	std::sort(results.begin(), results.end(), [](const simple_bench::batch_result<1>& lhs, const simple_bench::batch_result<1>& rhs) {
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