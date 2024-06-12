sort
====

A more universal constexpr sorting function, using keys or comparators!

Features
--------
* Drop in constexpr replacement for `std::sort`.
* Contextually picks radix or counting sorts for keyed sorting.
* Contextually picks partitioning algorithm where sensible.
* Overall: can be upwards of 25% faster than `std::sort`!

Usage
-----

```c
#include <iostream>
#include <vector>
#include <string_view>
#include "sort/sort.h"

/*
NOTE: Comparison functions are in the form
[](const auto& lhs, const auto &rhs) -> bool {...}

while keyed functions are in the form
[](const auto& current) -> integral_type {...}
or
[](const auto& current) -> std::tuple<integral_type,...> {...}

if a keyed function returns a bool, 
[](const auto& current) -> bool {...}
we'll partition the data, where items that return 
true are placed at the back!
*/

int main() {
	std::vector<int> example{42,31,99,3,4,5,96};
	//default ::std::less comparison
	sort::sort(example.begin(),example.end());
	for (const auto &value : example)
		std::cout << value << '\n'

	//NOTE: there's a performance cost to using larger keys!
	//return the smallest integral type that makes sense to use 

	//sort using count_sort implmentation / keyed values
	sort::sort(example.begin(),example.end(),[](const int& value){
		//sort based on swapped nibbles~
		return (value>>4) | (value<<4) & 0xf0;
	});
	for (const auto &value : example)
		std::cout << value << '\n'

	std::vector<std::string_view> strs{
		"hello",
		"world",
		"nice",
		"day",
		"right?"
	};

	//we can order any data based off of any calculation
	sort::sort(strs.begin(),strs.end(),[](const std::string_view& value){
		return value.size();
	});
	for (const auto &value : strs)
		std::cout << value << '\n'

	//or for more complicated structs...off of tuples
	sort::sort(strs.begin(),strs.end(),[](const std::string_view& value){
		return std::make_tuple(value.size()); //this is a bit redundant but that's ok
	});
	for (const auto &value : strs)
		std::cout << value << '\n'
	
	//if we use a keyed type of bool we'll partition data such that positions that
	//return true are placed towards the back 
	//(to be consistant with treating counting_sort's behavior if handed a uint8_t)
	sort::sort(strs.begin(),strs.end(),[](const std::string_view& value){
		return value.size() && value[0]=='n';
	});
	for (const auto &value : strs)
		std::cout << value << '\n'

	return 0;
}
```