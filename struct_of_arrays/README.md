# struct_of_arrays Library for C++

`struct_of_arrays` is designed to optimize memory access patterns by storing data in a struct of arrays (SoA) format. This allows for better cache utilization and improved performance, especially for data-intensive applications.

## Features
- **Efficient and Configurable Memory Layout:** Organizes data into arrays based on a sort function of your choice, by default places trivial data first and then packs data.
- **Improved Performance:** Single allocation for all provided types.

## Installation
To use `struct_of_arrays`, simply include the single header file in your project! Download or clone the repository and incorporate it directly.

### CMake Single Headers
```cmake
include(FetchContent)

FetchContent_Declare(
    open_source
    GIT_REPOSITORY https://github.com/Andersama-Library/open-source.git
    GIT_BRANCH c++
)

FetchContent_MakeAvailable(open_source)
include_directories(${open_source_SOURCE_DIR}/struct_of_arrays)
# we should now be able to #include "struct_of_arrays/struct_of_arrays.h"
```

## Usage
```cpp
#include "struct_of_arrays/struct_of_arrays.h"
#include <iostream>

int main(void) {
	struct_of_arrays::struct_of_arrays<int, std::string_view> keyed_struct{
					{0, "idx_0"}, {6, "idx_6"}, {7, "idx_7"}, {10, "idx_10"},
					{3, "idx_3"},
					{4, "idx_4"},
					{8, "idx_8"}, {11, "idx_11"}
	};

	struct_of_arrays::sort(
					keyed_struct, [](const auto& lhs, const auto& rhs) { return std::get<0>(lhs) < std::get<0>(rhs); });

	for (const auto& t : keyed_struct) {
		std::cout << ::std::get<0>(t) << " " << ::std::get<1>(t) << '\n';
	}

	return 0;
}
```

<!--For more detailed usage examples, please refer to the [Documentation](#documentation) section.-->

<!--## Documentation-->

<!--## Performance
[TODO: Include any performance benchmarks or comparisons with other libraries]-->

## Contributing
Feedback is welcome! See the discussions tab, or open up an issue.

## License
This library is licensed under a custom license for personal and commercial use. See the provided [LICENSE](LICENSE.md) file for details.

## Support
If you encounter any issues or have questions, feel free to [open an issue](https://github.com/Andersama-Library/open-source/issues)

## Acknowledgements
- **[https://tristanbrindle.com/posts/a-more-useful-compile-time-quicksort](https://tristanbrindle.com/posts/a-more-useful-compile-time-quicksort)** For a drop-in constexpr replacement of std::sort

## Sponsorware
When I meet different sponsorship amounts per month I'll move contents from my sponsorware repository into this one. Feel free to contact me for library ideas you'd like to see on the dicussions tab! If you're the first person to suggest an idea and I think it deserves to be sponsorware I'll make sure you have access to it when I start working on it!
There will be tiers listed on [github sponsors](https://github.com/sponsors/Andersama) for access to the sponsorware repository.

The sponsorware repository is licensed for both personal and commercial use.
**Important** Please note sponsoring me provides timed access via github sponsors, however I will be giving people permanent access as I'm offering perpectual licenses. If you lose access please email me.

## Roadmap
- [ ] **utility functionality** Currently due to the design which makes use of proxy references, some useful standard library functions are bugged.
- [x] **c++17 support* 