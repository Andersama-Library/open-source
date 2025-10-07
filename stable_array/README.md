stable_array
===================

This is a version of `std::vector` which maintains pointer stability where possible. It stores a history of its allocations.

This allows it to be used to keep references to data even as the container expands.

## NOTE!

Compared to a vector `data()` returns a `T**` pointer as opposed to a `T*`!
A utility function `where(uint64_t idx)` returns indexs for retrieving pointers and references from the internals of the structure using the `data()` for this reason.

```c
stable_array::stable_array<uint32_t> values;
values.emplace_back(100);
uint32_t* v = values.data()[0]+0; //access front()

auto w = values.where(0); //calculates indexs to retrieve the nth element
uint32_t* v2 = values.data()[w.blk_idx]+w.el_idx;

for (size_t i = 1; i < 100; i++) {
	values.emplace_back(i);
}

//Unless pointer stability has been violated via resize operations the address where v resides remains the same
//*v remains safe as long as erasing operations haven't destroyed v in the meantime.

std::cout << *v << '\n';

//For example here now we theoretically have a "safe" handle or pointer
//to the first element, and we can freely modify it and make use of it
//at will without repeating the work required to retrieve it from the
//container because its address will not change
*v = *v ^ *v; //set to 0

//Pointer stability also means we have the same garuntees for references

uint32_t &latest = values.emplace_back(42);
uint32_t &etc    = values.emplace_back(0xdeadbeef);

//unlike a vector accesses to latest, etc... will not dangle
//when values needs to aquire more space
```

## DANGER!

`shrink_to_fit()`

Will violate pointer stability, use STRICTLY when you know you can safely make the container smaller!

`constexpr void resize(size_t count)`

`constexpr void resize(size_t count, const value_type& value)`

Are programmed to erase when the count is smaller which will retain pointer stability, but they are still dangerous since `erase()` operations are destructive and may destroy elements which are still in use.