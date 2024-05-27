arguments
=========

A simple struct to refer to a program's arguments avoiding the cost of strlen() where possible.

Usage
-----

```c
	#include "arguments/arguments.h"
	#include <vector>

	void process_args(const std::vector<std::string_view> &args) {
		for (arg : args) {
			if (arg == "-h") {
				std::cout << "Printing Help!\n"
			} else {
				std::cout << "Not Printing Help!\n"
			}
		}
	}

	void process_args(arguments args) {
	    using namespace std::literals;
		for (size_t i = 0; i < args.argc; i++) {
			if (args.argument(i) == "-h"sv) {
				std::cout << "Printing Helpfullist!\n"
			} else {
				std::cout << "Not Printing Help!\n"
			}
		}
	}

	int main(int argc, char** argv) {
		arguments args{argc, argv};
		for (size_t i = 0; i < argc; i++) {
			std::cout << args.argument(i) << '\n';
		}
		
		// save arguments into a dynamic struct 
		std::vector<std::string_view> saved_args;
		for (size_t i = 0; i < argc; i++) {
			saved_args.emplace_back(args.argument(i));
		}

		process_args(saved_args);
		process_args(args);
		return 0;
	}
```
