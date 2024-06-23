#pragma once
#include <cstdint>
#include <array>
#include <tuple>
#include <type_traits>
#include <functional>

/*
MIT License

Copyright (c) 2024 Andersama-Library https://github.com/Andersama-Library/open-source

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef always_force_inline
#if __GNUC__
#define always_force_inline __attribute__((always_inline))
#elif __clang__
#define always_force_inline __attribute__((always_inline))
#elif _MSC_VER
#define always_force_inline __forceinline
#elif __EMSCRIPTEN__
#define always_force_inline
#elif __MINGW32__
#define always_force_inline
#elif __MINGW64__
#define always_force_inline
#else
#define always_force_inline
#endif
#endif

#if defined(NDEBUG) && NDEBUG
#define release_force_inline always_force_inline
#else
#define release_force_inline
#endif

#ifdef __cplusplus
#if defined(_MSVC_LANG) && _MSVC_LANG > __cplusplus
#define cplusplus_version_MACRO _MSVC_LANG
#else
#define cplusplus_version_MACRO _MSVC_LANG
#endif

#if cplusplus_version_MACRO >= 201402L
#define cplusplus_version_14 cplusplus_version_MACRO
#define constexpr_14 constexpr
#else
#undef cplusplus_version_14
#define constexpr_14
#endif

#if cplusplus_version_MACRO >= 201703L
#define cplusplus_version_17 cplusplus_version_MACRO
#define constexpr_17 constexpr
#else
#undef cplusplus_version_17
#define constexpr_17
#endif

#if cplusplus_version_MACRO >= 202002L
#define cplusplus_version_20 cplusplus_version_MACRO
#define constexpr_20 constexpr
#else
#undef cplusplus_version_20
#define constexpr_20
#endif
#endif

#if defined(cplusplus_version_20)
#include <bit>
#endif

namespace sort {
	template<typename It, typename Comp> struct comparator_info {
		using comparator_value_type = decltype(*std::declval<It>());
		static constexpr bool is_comparator =
						::std::is_invocable<Comp, comparator_value_type, comparator_value_type>::value;
		static constexpr bool is_keyed     = ::std::is_invocable<Comp, comparator_value_type>::value;
		static constexpr bool is_partition = ::std::is_invocable<bool, Comp, comparator_value_type>::value;
	};

	template<class T> using remove_cvref_t = typename std::remove_cv_t<std::remove_reference_t<T>>;
#if defined(cplusplus_version_20)
	template<class T> using iter_difference_t = ::std::iter_difference_t<T>;

	template<class T> using iter_value_t = ::std::iter_value_t<T>;
#else // defined(cplusplus_version_17) || defined(cplusplus_version_14)
	template<class T> using iter_difference_t = typename std::iterator_traits<sort::remove_cvref_t<T>>::difference_type;

	template<class T> using iter_value_t = typename std::iterator_traits<sort::remove_cvref_t<T>>::value_type;
#endif

	constexpr size_t insertion_sort_threshold = 32;
	constexpr size_t intro_sort_threshold     = 128;

	template<typename T, typename = void> struct is_pointer_convertible_iterator {
		static constexpr bool value = false;
	};

	template<typename T>
	struct is_pointer_convertible_iterator<T,
					typename std::enable_if<::std::is_convertible<T,
									typename ::std::iterator_traits<T>::pointer>::value>::type> {
		static constexpr bool value = true;
	};

	template<typename T>
	struct is_pointer_convertible_iterator<T,
					typename std::enable_if<::std::is_convertible<T,
									typename ::std::iterator_traits<T>::value_type*>::value>::type> {
		static constexpr bool value = true;
	};

	template<typename T, typename = void> struct is_msvc_unwrappable_iterator {
		static constexpr bool value = false;
	};

	template<typename T>
	struct is_msvc_unwrappable_iterator<T,
					typename std::enable_if<::std::is_convertible<
									decltype(::std::declval<sort::remove_cvref_t<T>>()._Unwrapped()),
									typename ::std::iterator_traits<typename sort::remove_cvref_t<T>>::value_type*>::
													value>::type> {
		static constexpr bool value = true;
	};

	template<typename T, typename = void> struct is_pointer_address_castable_iterator {
		static constexpr bool value = false;
	};

	template<typename> struct is_tuple : std::false_type {};
	template<typename... T> struct is_tuple<std::tuple<T...>> : std::true_type {};

	template<typename T = void> struct identity_less_than {
		[[nodiscard]] constexpr T&& operator()(T&& v) const noexcept
		{
			return ::std::forward<T>(v);
		}
		[[nodiscard]] constexpr const T& operator()(const T& v) const noexcept
		{
			return v;
		}
	};

	template<> struct identity_less_than<void> {
		template<class T> [[nodiscard]] constexpr T&& operator()(T&& v) const noexcept
		{
			return ::std::forward<T>(v);
		}
		template<class T> [[nodiscard]] constexpr const T& operator()(const T& v) const noexcept
		{
			return v;
		}
	};

	template<typename T = void> struct identity_greater_than {
		[[nodiscard]] constexpr T&& operator()(T&& v) const noexcept
		{
			return ::std::forward<T>(v);
		}
		[[nodiscard]] constexpr const T& operator()(const T& v) const noexcept
		{
			return v;
		}
	};

	template<> struct identity_greater_than<void> {
		template<class T> [[nodiscard]] constexpr T&& operator()(T&& v) const noexcept
		{
			return ::std::forward<T>(v);
		}
		template<class T> [[nodiscard]] constexpr const T& operator()(const T& v) const noexcept
		{
			return v;
		}
	};

	template<class It> [[nodiscard]] constexpr decltype(auto) get_unwrapped(It&& it)
	{
		bool convertible = is_pointer_convertible_iterator<It>::value;
		if constexpr (::std::is_pointer_v<::std::decay_t<It>>) { // special-case pointers and arrays
			return it + 0;
		} else if constexpr (is_pointer_convertible_iterator<It>::value) {
			return static_cast<typename ::std::iterator_traits<It>::pointer_type>(it);
		} else if constexpr (is_msvc_unwrappable_iterator<It>::value) {
			return static_cast<It&&>(it)._Unwrapped();
		} else {
			return static_cast<It&&>(it);
		}
	}

	template<typename It> [[nodiscard]] constexpr auto distance(It start, It end)
	{
		if constexpr (::std::is_convertible_v<typename ::std::iterator_traits<It>::iterator_category,
									  ::std::random_access_iterator_tag>) {
			return end - start;
		} else {
			auto      f    = get_unwrapped(start);
			auto      l    = get_unwrapped(end);
			ptrdiff_t diff = 0;
			for (; f != l; ++f)
				++diff;
			return diff;
		}
	}

	template<class RAIt, class Compare = std::less<>> constexpr void insertion_sort(RAIt, RAIt, Compare);

	template<typename It, typename Compare = std::less<>> constexpr void intro_sort(It, It, Compare, size_t);

	template<typename It, typename Compare = std::less<>> constexpr void make_heap(It, It, Compare);

	template<typename It, typename Compare = std::less<>> constexpr void sort_heap(It, It, Compare);

	template<typename It> constexpr It prev_iter(It it)
	{
		return --it;
	}

	template<typename It> constexpr It next_iter(It it)
	{
		return ++it;
	}

	template<class ForwardIt1, class ForwardIt2>
	release_force_inline constexpr void iter_swap(ForwardIt1 a, ForwardIt2 b)
	{
		using value_type  = sort::remove_cvref_t<typename std::iterator_traits<ForwardIt1>::value_type>;
		using value_type2 = sort::remove_cvref_t<typename std::iterator_traits<ForwardIt2>::value_type>;
		if constexpr (::std::is_trivial<value_type>::value) {
			auto temp = *a;
			*a        = *b;
			*b        = temp;
		} else if constexpr (::std::is_invocable<decltype(&(a->swap)),
											 value_type2&>::value) { // use type's provided swap function if it exists
			(*a).swap(*b);
		} else if constexpr (::std::is_swappable_with<value_type&,
											 value_type2&>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			swap(*a, *b);
		} else {
			static_assert(false, "iter swap requires that the dereferenced types are swappable!");
		}
	}

	template<typename T> release_force_inline constexpr void swap_branchless_unconditional(T& lhs, T& rhs)
	{
		if constexpr (::std::is_trivial<T>::value) {
			auto tmp = lhs;
			lhs      = rhs;
			rhs      = tmp;
		} else if constexpr (::std::is_invocable<decltype(&(lhs.swap)),
											 T&>::value) { // use type's provided swap function if it exists
			lhs.swap(rhs);
		} else if constexpr (::std::is_swappable_with<T&,
											 T&>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			swap(lhs, rhs);
		} else {
			static_assert(false, "swap_branchless_unconditional requires that the types are swappable!");
		}
	}

	template<typename T> release_force_inline constexpr void swap_branchless_conditional(T& lhs, T& rhs, bool c)
	{
		if constexpr (::std::is_trivial<T>::value) {
			T tmp[2] = {lhs, rhs};
			lhs      = tmp[c];
			rhs      = tmp[!c];
		} else if constexpr (::std::is_invocable<decltype(&(lhs.swap)),
											 T&>::value) { // use type's provided swap function if it exists
			if (c)
				lhs.swap(rhs);
		} else if constexpr (::std::is_swappable_with<T&,
											 T&>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			if (c)
				swap(lhs, rhs);
		} else {
			static_assert(false, "swap_branchless_unconditional requires that the types are swappable!");
		}
	}

	template<class ForwardIt, class UnaryPred>
	constexpr ForwardIt partition(ForwardIt first, ForwardIt last, UnaryPred p)
	{
		for (; first != last; first++) {
			if (!p(*first))
				break;
		}
		if (first == last)
			return first;
		auto f = first;
		for (auto i = ++f; i != last; ++i) {
			if (p(*i)) {
				sort::iter_swap(i, first);
				++first;
			}
		}

		return first;
	}

	template<class ForwardIt, class UnaryPred>
	constexpr ForwardIt reversed_partition(ForwardIt first, ForwardIt last, UnaryPred p)
	{
		for (; first != last; first++) {
			if (p(*first))
				break;
		}
		if (first == last)
			return first;
		auto f = first;
		for (auto i = ++f; i != last; ++i) {
			if (!p(*i)) {
				sort::iter_swap(i, first);
				++first;
			}
		}

		return first;
	}

	struct counting_sort_bytes {
		uint64_t idxs  = {};
		uint8_t  bytes = {};
	};

	// see: https://github.com/skarupke/ska_sort/blob/master/ska_sort.hpp as reference
	template<typename It, typename ExtractKey>
	release_force_inline constexpr void counting_sort_byte_shift(
					It start, It end, ExtractKey extract_key, counting_sort_bytes remaining)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(*std::declval<It>()))>;
		static_assert(std::is_integral<key_type>::value, "extract_key must return an integral type!");

		uint32_t bit_shift = (remaining.idxs & 0xff) * 8;
		--remaining.bytes;
		remaining.idxs >>= 8;

		size_t counts[256] = {0};
		// The start of one index is the end of another, we can compress the data into
		// index pairs right next to each other
		size_t start_end[256 + 1];
		{
			key_type mn = ~key_type{0};
			key_type mx = key_type{0};

			for (It it = start; it != end; ++it) {
				key_type k = extract_key(*it);
				++counts[(k >> bit_shift) & 0xff];
				mn = k < mn ? k : mn;
				mx = k > mx ? k : mx;
			}

			key_type diff = mx - mn;
			switch (diff) {
			case 1:
				sort::partition(start, end, [&mx](const auto& v) { return ExtractKey{}(v) < mx; });
			case 0:
				return;
			default:
				break;
			}
		}

		size_t total = 0;
		// convert our counts to starting indexs
		// IE, if we know we've seen 4 of some value
		// then the current total is the index these values
		// would start at and we know the total would increase by 4
		uint32_t partitions = 0;
		{
			size_t idx = 0;
			for (size_t& count : counts) {
				size_t old_count = count;
				partitions += count > 0;
				count          = total;
				start_end[idx] = total;
				total += old_count;
				idx++;
			}
			start_end[256] = total;
		}
		// from skarupke's 2017 video, instead of sorting via the start of the array and
		// continuously swapping the first element until it's in place
		// instead we flip the algorithm around from the index's perspective
		// NOTE: how this algorithm sorts, we create an array that gives away the end
		//  state of how the array will be sorted! We can use this to a huge advantage!
		//  While "count" is initially used to count how many items belong in each bin
		//  we use it a second time to keep track of where to swap an item into that bin*
		//  This means we effectively have 256 stack local vectors where:
		//  count[key] < start_end[key+1] means we're not done sorting because
		//  everything from start_end[key] -> count[key] is sorted
		//  and count[key] -> start_end[key+1] might not be...
		// BIG NOTE: Meaning start[key] to count[key] are sorted exactly where they want to be!
		//  so instead of the cpu trampling all over itself we strictly
		//  swap indexs we know are guaranteed to be out of position, so with minimal
		//  book-keeping we only swap each out of position item once, this guarantees
		//  we avoid memory dependencies on previous swaps! Allowing the cpu to absolutely blitz through
		//  each swap and predict the remainder of the loops well in advance
		if (partitions > 1) {
			size_t sorted_count = 0;
			do {
				for (size_t x = 0; x < 256; x++) {
					size_t s = counts[x];
					size_t e = start_end[x + 1];
					// this is so when we loop back around we start past the point we
					// know the data is sorted, skarupke mention's swapping things around
					// I'm not convinced that's a good idea, plus this is easy to program anyway
					sorted_count += (e - s);
					for (; s < e; s++) {
						It      swap_left = start + s;
						It      swap_target;
						uint8_t key        = (extract_key(*swap_left) >> bit_shift) & 0xff;
						size_t  target_idx = counts[key];
						swap_target        = start + target_idx;

						swap_branchless_unconditional(*swap_left, *swap_target);
						counts[key] += 1;
					}
				}
			} while (sorted_count < start_end[256]);
		}

		// the recursion step
		if (remaining.bytes) {
			for (uint32_t i = 0; i < 256; i++) {
				size_t items = (start_end[i + 1] - start_end[i]);
				if (items <= insertion_sort_threshold) {
					sort::insertion_sort(start + start_end[i], start + start_end[i + 1],
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else if (items <= intro_sort_threshold) {
					sort::make_heap(start + start_end[i], start + start_end[i + 1],
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
					sort::sort_heap(start + start_end[i], start + start_end[i + 1],
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else {
					sort::counting_sort_byte_shift(
									start + start_end[i], start + start_end[i + 1], extract_key, remaining);
				}
			}
		}
	}

	template<typename It, typename ExtractKey>
	release_force_inline constexpr void counting_sort_byte_shift_dynamic(It start, It end, ExtractKey extract_key)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(*std::declval<It>()))>;
		static_assert(std::is_integral<key_type>::value, "extract_key must return an integral type!");

		size_t counts[256] = {0};
		// The start of one index is the end of another, we can compress the data into
		// index pairs right next to each other
		size_t   start_end[256 + 1];
		key_type mn                    = ~key_type{0};
		key_type mx                    = key_type{0};
		uint8_t  mxs[sizeof(key_type)] = {0};
		uint8_t  mns[sizeof(key_type)];

		for (uint8_t& mn : mns)
			mn = ~uint8_t{0};

		for (It it = start; it != end; ++it) {
			key_type k = extract_key(*it);
			for (size_t x = 0; x < sizeof(key_type); x++) {
				uint8_t key_byte = ((k >> (x * 8)) & 0xff);
				mns[x]           = key_byte < mns[x] ? key_byte : mns[x];
				mxs[x]           = key_byte > mxs[x] ? key_byte : mxs[x];
			}
			mn = k < mn ? k : mn;
			mx = k > mx ? k : mx;
		}

		// array of all the same values
		{
			key_type diff = mx - mn;
			switch (diff) {
			case 1:
				sort::partition(start, end, [&mx](const auto& v) { return ExtractKey{}(v) < mx; });
			case 0:
				return;
			default:
				break;
			}
		}

		counting_sort_bytes next{};
		// find the first byte that we can sort off of
		/// uint32_t bytes_needing_sorting = 0;
		for (uint32_t x = sizeof(key_type); --x < sizeof(key_type);) {
			bool bit_range = mns[x] != mxs[x];
			next.idxs |= (x << (8 * next.bytes)) * bit_range;
			next.bytes += bit_range;
		}

		uint32_t bit_shift = (next.idxs & 0xff) * 8;
		--next.bytes;
		next.idxs >>= 8;

		// size_t diff = mxs[bit_shift>>3] - mn[bit_shift>>3];
		// if (diff == 0)
		//	return;
		// if (diff == 1)
		//	return sort::partition(start, end, [&mn](const auto& v) { return mn <= ExtractKey{}(v); });
		for (It it = start; it != end; ++it) {
			key_type k   = extract_key(*it);
			uint8_t  idx = (k >> bit_shift) & 0xff;
			++counts[idx];
		}
		// convert our counts to starting indexs
		// IE, if we know we've seen 4 of some value
		// then the current total is the index these values
		// would start at and we know the total would increase by 4
		// uint32_t partitions = 0;
		{
			size_t idx   = 0;
			size_t total = 0;
			for (size_t& count : counts) {
				size_t old_count = count;
				// partitions += old_count > 0;
				count          = total;
				start_end[idx] = total;
				total += old_count;
				idx++;
			}
			start_end[256] = total;
		}

		// from skarupke's 2017 video, instead of sorting via the start of the array and
		// continuously swapping the first element until it's in place
		// instead we flip the algorithm around from the index's perspective
		// NOTE: how this algorithm sorts, we create an array that gives away the end
		//  state of how the array will be sorted! We can use this to a huge advantage!
		//  While "count" is initially used to count how many items belong in each bin
		//  we use it a second time to keep track of where to swap an item into that bin*
		//  This means we effectively have 256 stack local vectors where:
		//  count[key] < start_end[key+1] means we're not done sorting because
		//  everything from start_end[key] -> count[key] is sorted
		//  and count[key] -> start_end[key+1] might not be...
		// BIG NOTE: Meaning start[key] to count[key] are sorted exactly where they want to be!
		//  so instead of the cpu trampling all over itself we strictly
		//  swap indexs we know are guaranteed to be out of position, so with minimal
		//  book-keeping we only swap each out of position item once, this guarantees
		//  we avoid memory dependencies on previous swaps! Allowing the cpu to absolutely blitz through
		//  each swap and predict the remainder of the loops well in advance
		if ((mxs[bit_shift >> 3] - mns[bit_shift >> 3]) > 1) {
			size_t sorted_count = 0;
			do {
				for (size_t x = 0; x < 256; x++) {
					size_t s = counts[x];
					size_t e = start_end[x + 1];
					// this is so when we loop back around we start past the point we
					// know the data is sorted, skarupke mention's swapping things around
					// I'm not convinced that's a good idea, plus this is easy to program anyway
					sorted_count += (e - s);
					for (; s < e; s++) {
						It      swap_left = start + s;
						It      swap_target;
						uint8_t key        = (extract_key(*swap_left) >> bit_shift) & 0xff;
						size_t  target_idx = counts[key];
						swap_target        = start + target_idx;

						sort::swap_branchless_unconditional(*swap_left, *swap_target);
						counts[key] += 1;
					}
				}
			} while (sorted_count < start_end[256]);
		}

		// the recursion step
		if (next.bytes) {
			for (uint32_t i = 0; i < 256; i++) {
				size_t items = (start_end[i + 1] - start_end[i]);
				if (items <= insertion_sort_threshold) {
					sort::insertion_sort(start + start_end[i], start + start_end[i + 1],
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else if (items <= intro_sort_threshold) {
					sort::make_heap(start + start_end[i], start + start_end[i + 1],
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
					sort::sort_heap(start + start_end[i], start + start_end[i + 1],
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else {
					sort::counting_sort_byte_shift(start + start_end[i], start + start_end[i + 1], extract_key, next);
				}
			}
		}
	}

	template<typename It, typename ExtractKey>
	constexpr void counting_sort_byte_shift_flat(It start, It end, ExtractKey extract_key)
	{
		using key_type   = sort::remove_cvref_t<decltype(ExtractKey{}(*std::declval<It>()))>;
		using index_type = size_t;
		static_assert(std::is_integral<key_type>::value, "extract_key must return an integral type!");
		constexpr size_t initial_count_indexs      = 256 * sizeof(key_type);
		constexpr size_t required_start_end_indexs = 257 * sizeof(key_type);
		constexpr size_t count_indexs              = 256;
		constexpr size_t start_end_indexs          = 257;

		// The start of one index is the end of another, we can compress the data into
		// index pairs right next to each other
		std::array<index_type, required_start_end_indexs> stack_data = {};
		std::array<index_type, count_indexs>              counts; // we can reuse these for each recursion depth

		std::array<uint16_t, sizeof(key_type)> progress = {};
		std::array<It, sizeof(key_type)>       its;

		key_type mn                    = ~key_type{0};
		key_type mx                    = key_type{0};
		uint8_t  mxs[sizeof(key_type)] = {0};
		uint8_t  mns[sizeof(key_type)];

		for (uint8_t& mn : mns)
			mn = ~uint8_t{0};

		for (It it = start; it != end; ++it) {
			key_type k = extract_key(*it);

			mn = k < mn ? k : mn;
			mx = k > mx ? k : mx;

			for (size_t x = 0; x < sizeof(key_type); x++) {
				uint8_t key_byte = ((k >> (x * 8)) & 0xff);
				mns[x]           = key_byte < mns[x] ? key_byte : mns[x];
				mxs[x]           = key_byte > mxs[x] ? key_byte : mxs[x];

				++stack_data[count_indexs * x + key_byte];
			}
		}

		// array of all the same values
		{
			key_type diff = mx - mn;
			switch (diff) {
			case 1:
				sort::partition(start, end, [&mx](const auto& v) { return ExtractKey{}(v) < mx; });
			case 0:
				return;
			default:
				break;
			}
		}

		counting_sort_bytes next{};
		uint8_t             depth = 0;
		// find the first byte that we can sort off of
		/// uint32_t bytes_needing_sorting = 0;
		for (uint32_t x = sizeof(key_type); --x < sizeof(key_type);) {
			bool bit_range = mns[x] != mxs[x];
			next.idxs |= (x << (8 * next.bytes)) * bit_range;
			next.bytes += bit_range;
		}

		its[0]              = start;
		uint32_t partitions = 0;
		{ // convert counts to prefix sum
			size_t   idx          = 0;
			size_t   total        = 0;
			uint16_t current_byte = (next.idxs >> (depth * 8)) & 0xff;
			for (; idx < 256;) {
				size_t count = stack_data[(current_byte * count_indexs) + idx];

				size_t old_count = count;
				partitions += old_count > 0;
				counts[idx]     = total;
				stack_data[idx] = total;
				total += old_count;
				idx++;
			}
			stack_data[256] = total;
		}

		switch (partitions) {
		case 2:
			sort::partition(start, end, [&mx](const auto& v) { return ExtractKey{}(v) < mx; });
			[[fallthrough]];
		case 1:
			[[fallthrough]];
		case 0:
			return;
		default:
			break;
		}

		auto start_it = its[depth];

		uint32_t bit_shift    = ((next.idxs >> (depth * 8)) & 0xff) * 8;
		size_t   sorted_count = 0;
		do {
			for (size_t x = 0; x < 256; x++) {
				size_t s = counts[x];                                      // counts[depth][x];
				size_t e = stack_data[(depth * start_end_indexs) + x + 1]; // start_end[depth][x + 1];
				// this is so when we loop back around we start past the point we
				// know the data is sorted, skarupke mention's swapping things around
				// I'm not convinced that's a good idea, plus this is easy to program anyway
				sorted_count += (e - s);
				for (; s < e; s++) {
					It      swap_left = start_it + s;
					It      swap_target;
					uint8_t key        = (extract_key(*swap_left) >> bit_shift) & 0xff;
					size_t  target_idx = counts[key];
					swap_target        = start_it + target_idx;

					sort::swap_branchless_unconditional(*swap_left, *swap_target);
					counts[key] += 1;
				}
			}
		} while (sorted_count < stack_data[(depth * start_end_indexs) + 256]);
		// no recursion needed, every item had a dedicated location
		if (partitions == (end - start) || next.bytes <= 1)
			return;

		for (;;) {
			//  the recursion step
			for (; progress[depth] < 256; ++progress[depth]) {
				uint16_t i            = progress[depth];
				size_t   start_offset = stack_data[(depth * start_end_indexs) + i];
				size_t   end_offset   = stack_data[(depth * start_end_indexs) + i + 1];
				size_t   items        = end_offset - start_offset;
				// skip where we have 0 items to process
				if (items <= insertion_sort_threshold) {
					// this should "bubble" up, marking this region as definitely sorted
					sort::insertion_sort(start_it + start_offset, start_it + end_offset,
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else if (items <= intro_sort_threshold) {
					// this should "bubble" up, marking this region as definitely sorted
					sort::make_heap(start_it + start_offset, start_it + end_offset,
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
					sort::sort_heap(start_it + start_offset, start_it + end_offset,
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
					// } else if (depth < depth_limit) {
				} else {
					mn = ~key_type{0};
					mx = key_type{0};
					// do normal count
					uint16_t next_depth = depth + 1;
					It       end_it     = start_it + end_offset;
					// setup to process the next depth
					for (size_t& count : counts)
						count = 0;

					uint32_t bit_shift = ((next.idxs >> (next_depth * 8)) & 0xff) * 8;
					for (It it = start_it + start_offset; it != end_it; ++it) {
						key_type k = extract_key(*it);

						mn               = k < mn ? k : mn;
						mx               = k > mx ? k : mx;
						uint8_t key_byte = k >> bit_shift;
						++counts[key_byte];
					}

					key_type diff = mx - mn;
					switch (diff) {
					case 1:
						sort::partition(start_it + start_offset, end_it,
										[&mx](const auto& v) { return ExtractKey{}(v) < mx; });
						[[fallthrough]];
					case 0:
						continue;
					default:
						break;
					}

					uint32_t partitions = 0;
					{ // convert counts to prefix sum
						size_t idx   = 0;
						size_t total = 0;
						for (size_t& count : counts) {
							size_t old_count = count;
							partitions += old_count > 0;
							count                                             = total;
							stack_data[(next_depth * start_end_indexs) + idx] = total;
							counts[idx]                                       = total;
							total += old_count;
							idx++;
						}
						stack_data[(next_depth * start_end_indexs) + 256] = total;
					}

					its[next_depth] = start_it + start_offset;

					start_it = its[next_depth];

					size_t sorted_count = 0;
					do {
						for (size_t x = 0; x < 256; x++) {
							size_t s = counts[x];
							size_t e = stack_data[(next_depth * start_end_indexs) + x + 1];
							// this is so when we loop back around we start past the point we
							// know the data is sorted, skarupke mention's swapping things around
							// I'm not convinced that's a good idea, plus this is easy to program anyway
							sorted_count += (e - s);
							for (; s < e; s++) {
								It      swap_left = start_it + s;
								It      swap_target;
								uint8_t key        = (extract_key(*swap_left) >> bit_shift) & 0xff;
								size_t  target_idx = counts[key];
								swap_target        = start_it + target_idx;

								sort::swap_branchless_unconditional(*swap_left, *swap_target);
								counts[key] += 1;
							}
						}
					} while (sorted_count < stack_data[(next_depth * start_end_indexs) + 256]); // start_end[256]
					// we don't need to recurse if the # of items matches, or we're at the max depth already
					if (partitions == items || (next_depth >= (next.bytes - 1)))
						continue;

					progress[next_depth] = 0;
					break; // we'll go deeper
				}
			}

			if (depth == 0 && progress[0] >= 256)
				break;
			progress[depth] += 1;
			depth += (progress[depth] < 256) ? -1 : 1;
		}
	}

	template<typename T> always_force_inline constexpr auto treat_as_unsigned(T v)
	{
#if defined(cplusplus_version_20)
		if constexpr (::std::is_floating_point<T>::value) {
			if constexpr (::std::is_same<T, float>::value) {
				uint32_t uv        = std::bit_cast<uint32_t>(v);
				uint32_t sf        = uv >> (sizeof(uint32_t) * 8 - 1);
				uint32_t flip_mask = 0x80000000 | (0xffffffff * sf);
				return uv ^ flip_mask;
			} else if constexpr  (::std::is_same<T, double>::value) {
				uint64_t uv        = ::std::bit_cast<uint64_t>(v);
				uint64_t sf        = uv >> (sizeof(uint64_t) * 8 - 1);
				uint32_t flip_mask = 0x8000000000000000 | (0xffffffffffffffff * sf);
				return uv ^ flip_mask;
			} else {
				static_assert(false, "type needs to be convertible to an unsigned integer to be used as a key, floating types float and double are supported");
			}
		} else
#endif
		if constexpr (::std::is_integral<T>::value && ::std::is_signed<T>::value) {
			constexpr typename ::std::make_unsigned<T>::type min_value = {
				~(~typename ::std::make_unsigned<T>::type {0} >> 1)
			};
			return (typename ::std::make_unsigned<T>::type)v + min_value;
		} else if constexpr (::std::is_integral<T>::value) {
			return v;
		} else {
			static_assert(false, "type needs to be convertible to an unsigned integer to be used as a key");
			return v;
		}
	}

	template<typename It, typename ExtractKey, size_t Idx, size_t... Idxs>
	constexpr void counting_sort_get_impl(It start, It end, ExtractKey extract_key, std::index_sequence<Idx, Idxs...>)
	{
		using key_type          = sort::remove_cvref_t<decltype(::std::get<Idx>(ExtractKey{}(*std::declval<It>())))>;
		using unsigned_key_type = typename ::std::make_unsigned<key_type>::type;
		using index_type        = size_t;
		// static_assert(std::is_integral<key_type>::value,
		//				"::std::get<Idx>(extract_key(*it)) must return an integral type!");
		constexpr size_t initial_count_indexs      = 256 * sizeof(key_type);
		constexpr size_t required_start_end_indexs = 257 * sizeof(key_type);
		constexpr size_t count_indexs              = 256;
		constexpr size_t start_end_indexs          = 257;

		// The start of one index is the end of another, we can compress the data into
		// index pairs right next to each other
		std::array<index_type, required_start_end_indexs> stack_data = {};
		std::array<index_type, count_indexs>              counts; // we can reuse these for each recursion depth

		std::array<uint16_t, sizeof(key_type)> progress = {};
		std::array<It, sizeof(key_type)>       its;

		unsigned_key_type mn                    = ~unsigned_key_type{0};
		unsigned_key_type mx                    = unsigned_key_type{0};
		uint8_t           mxs[sizeof(key_type)] = {0};
		uint8_t           mns[sizeof(key_type)];

		for (uint8_t& mn : mns)
			mn = ~uint8_t{0};

		for (It it = start; it != end; ++it) {
			key_type          k  = ::std::get<Idx>(extract_key(*it));
			unsigned_key_type uk = sort::treat_as_unsigned(k);

			mn = uk < mn ? uk : mn;
			mx = uk > mx ? uk : mx;

			for (size_t x = 0; x < sizeof(key_type); x++) {
				uint8_t key_byte = ((uk >> (x * 8)) & 0xff);
				mns[x]           = key_byte < mns[x] ? key_byte : mns[x];
				mxs[x]           = key_byte > mxs[x] ? key_byte : mxs[x];

				++stack_data[count_indexs * x + key_byte];
			}
		}

		// array of all the same values
		{
			unsigned_key_type diff = mx - mn;
			switch (diff) {
			case 1:
				sort::partition(start, end, [&mx](const auto& v) {
					return sort::treat_as_unsigned(::std::get<Idx>(ExtractKey{}(v))) < mx;
				});
			case 0:
				return;
			default:
				break;
			}
		}

		counting_sort_bytes next{};
		uint8_t             depth = 0;
		// find the first byte that we can sort off of
		/// uint32_t bytes_needing_sorting = 0;
		for (uint32_t x = sizeof(key_type); --x < sizeof(key_type);) {
			bool bit_range = mns[x] != mxs[x];
			next.idxs |= (x << (8 * next.bytes)) * bit_range;
			next.bytes += bit_range;
		}

		its[0]              = start;
		uint32_t partitions = 0;
		{ // convert counts to prefix sum
			size_t   idx          = 0;
			size_t   total        = 0;
			uint16_t current_byte = (next.idxs >> (depth * 8)) & 0xff;
			for (; idx < 256;) {
				size_t count = stack_data[(current_byte * count_indexs) + idx];

				size_t old_count = count;
				partitions += old_count > 0;
				counts[idx]     = total;
				stack_data[idx] = total;
				total += old_count;
				idx++;
			}
			stack_data[256] = total;
		}

		switch (partitions) {
		case 2:
			sort::partition(start, end, [&mx](const auto& v) {
				return sort::treat_as_unsigned(::std::get<Idx>(ExtractKey{}(v))) < mx;
			});
			[[fallthrough]];
		case 1:
			[[fallthrough]];
		case 0:
			return;
		default:
			break;
		}

		auto start_it = its[depth];

		uint32_t bit_shift    = ((next.idxs >> (depth * 8)) & 0xff) * 8;
		size_t   sorted_count = 0;
		do {
			for (size_t x = 0; x < 256; x++) {
				size_t s = counts[x];                                      // counts[depth][x];
				size_t e = stack_data[(depth * start_end_indexs) + x + 1]; // start_end[depth][x + 1];
				// this is so when we loop back around we start past the point we
				// know the data is sorted, skarupke mention's swapping things around
				// I'm not convinced that's a good idea, plus this is easy to program anyway
				sorted_count += (e - s);
				for (; s < e; s++) {
					It      swap_left = start_it + s;
					It      swap_target;
					uint8_t key = (sort::treat_as_unsigned(::std::get<Idx>(extract_key(*swap_left))) >> bit_shift) &
								  0xff;
					size_t target_idx = counts[key];
					swap_target       = start_it + target_idx;

					sort::swap_branchless_unconditional(*swap_left, *swap_target);
					counts[key] += 1;
				}
			}
		} while (sorted_count < stack_data[(depth * start_end_indexs) + 256]);
		// no recursion needed, every item had a dedicated location
		if (partitions == (end - start))
			return;
		if constexpr ((sizeof...(Idxs)) == 0) {
			if (next.bytes <= 1) // we have other parts of the key to extract
				return;
		}

		for (;;) {
			//  the recursion step
			for (; progress[depth] < 256; ++progress[depth]) {
				uint16_t i            = progress[depth];
				size_t   start_offset = stack_data[(depth * start_end_indexs) + i];
				size_t   end_offset   = stack_data[(depth * start_end_indexs) + i + 1];
				size_t   items        = end_offset - start_offset;
				// skip where we have 0 items to process
				if (items <= insertion_sort_threshold) {
					// this should "bubble" up, marking this region as definitely sorted
					sort::insertion_sort(start_it + start_offset, start_it + end_offset,
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else if (items <= intro_sort_threshold) {
					// this should "bubble" up, marking this region as definitely sorted
					sort::make_heap(start_it + start_offset, start_it + end_offset,
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
					sort::sort_heap(start_it + start_offset, start_it + end_offset,
									[](const auto& lhs, const auto& rhs) {
										return ExtractKey{}(lhs) < ExtractKey{}(rhs);
									});
				} else {
					uint16_t next_depth = depth + 1;
					if constexpr (sizeof...(Idxs)) {
						if (next_depth >= next.bytes) {
							// go one key deeper
							counting_sort_get_impl(start_it + start_offset, start_it + end_offset, extract_key,
											std::index_sequence<Idxs...>{});
							continue;
						}
					}

					mn = ~key_type{0};
					mx = key_type{0};
					// do normal count

					It end_it = start_it + end_offset;
					// setup to process the next depth
					for (size_t& count : counts)
						count = 0;

					uint32_t bit_shift = ((next.idxs >> (next_depth * 8)) & 0xff) * 8;
					for (It it = start_it + start_offset; it != end_it; ++it) {
						unsigned_key_type k = sort::treat_as_unsigned(::std::get<Idx>(extract_key(*it)));

						mn               = k < mn ? k : mn;
						mx               = k > mx ? k : mx;
						uint8_t key_byte = k >> bit_shift;
						++counts[key_byte];
					}

					unsigned_key_type diff = mx - mn;
					switch (diff) {
					case 1:
						sort::partition(start_it + start_offset, end_it,
										[&mx](const auto& v) { return ::std::get<Idx>(ExtractKey{}(v)) < mx; });
						[[fallthrough]];
					case 0:
						continue;
					default:
						break;
					}

					uint32_t partitions = 0;
					{ // convert counts to prefix sum
						size_t idx   = 0;
						size_t total = 0;
						for (size_t& count : counts) {
							size_t old_count = count;
							partitions += old_count > 0;
							count                                             = total;
							stack_data[(next_depth * start_end_indexs) + idx] = total;
							// start_end[next_depth][idx] = total;
							total += old_count;
							idx++;
						}
						stack_data[(next_depth * start_end_indexs) + 256] = total;
						// start_end[next_depth][256] = total;
					}

					its[next_depth] = start_it + start_offset;

					start_it = its[next_depth];

					size_t sorted_count = 0;
					do {
						for (size_t x = 0; x < 256; x++) {
							size_t s = counts[x];
							size_t e = stack_data[(next_depth * start_end_indexs) + x + 1];
							// this is so when we loop back around we start past the point we
							// know the data is sorted, skarupke mention's swapping things around
							// I'm not convinced that's a good idea, plus this is easy to program anyway
							sorted_count += (e - s);
							for (; s < e; s++) {
								It      swap_left = start_it + s;
								It      swap_target;
								uint8_t key = (sort::treat_as_unsigned(::std::get<Idx>(extract_key(*swap_left))) >>
															  bit_shift) &
											  0xff;
								size_t  target_idx = counts[key];
								swap_target        = start_it + target_idx;

								sort::swap_branchless_unconditional(*swap_left, *swap_target);
								counts[key] += 1;
							}
						}
					} while (sorted_count < stack_data[(next_depth * start_end_indexs) + 256]); // start_end[256]
					// we don't need to recurse if the # of items matches
					if constexpr ((sizeof...(Idxs)) == 0) {
						if (next_depth >= (next.bytes - 1))
							continue;
					}
					if (partitions == items)
						continue;
					progress[next_depth] = 0;
					break; // we'll go deeper
				}
			}

			if (depth == 0 && progress[0] >= 256) //
				break;
			progress[depth] += 1;

			depth += (progress[depth] < 256) ? -1 : 1;
		}
	}

	template<typename It, typename ExtractKey>
	release_force_inline constexpr void counting_sort_integral(It start, It end, ExtractKey extract_key)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(*std::declval<It>()))>;
		static_assert(std::is_integral<key_type>::value, "extract_key must return an integral type!");

		if constexpr (::std::is_signed<key_type>::value) {
			constexpr typename ::std::make_unsigned<key_type>::type min_value = {
							~(~typename ::std::make_unsigned<key_type>::type{0} >> 1)};
			sort::counting_sort_byte_shift_flat(start, end, [](const auto& value) {
				return (typename ::std::make_unsigned<key_type>::type)ExtractKey{}(value) + min_value;
			});
		} else {
			sort::counting_sort_byte_shift_flat(start, end, extract_key);
		}
	}

#if defined(cplusplus_version_20)
	constexpr uint32_t extract_float(float value)
	{
		uint32_t uv        = std::bit_cast<uint32_t>(value);
		uint32_t sf        = uv >> (sizeof(uint32_t) * 8 - 1);
		uint32_t flip_mask = 0x80000000 | (0xffffffff * sf);
		return uv ^ flip_mask;
	}

	constexpr uint64_t extract_double(double value)
	{
		uint64_t uv        = ::std::bit_cast<uint64_t>(value);
		uint64_t sf        = uv >> (sizeof(uint64_t) * 8 - 1);
		uint32_t flip_mask = 0x8000000000000000 | (0xffffffffffffffff * sf);
		return uv ^ flip_mask;
	}

	template<typename It, typename ExtractKey>
	constexpr void counting_sort_floating(It start, It end, ExtractKey extract_key)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(*std::declval<It>()))>;
		static_assert(std::is_floating_point<key_type>::value, "extract_key must return a floating point type!");

		if constexpr (::std::same_as<key_type, float>::value) {
			constexpr key_type r = ::std::numeric_limits<key_type>::min();
			sort::counting_sort_byte_shift(
							start, end,
							[](const auto& v) {
								float    value     = extract_key(v);
								uint32_t uv        = std::bit_cast<uint32_t>(value);
								uint32_t sf        = uv >> (sizeof(uint32_t) * 8 - 1);
								uint32_t flip_mask = 0x80000000 | (0xffffffff * sf);
								return uv ^ flip_mask;
							},
							(sizeof(key_type) * 8) - 8);
		} else if constexpr (::std::same_as<key_type, double>::value) {
			sort::counting_sort_byte_shift(
							start, end,
							[](const auto& v) {
								double   value     = extract_key(v);
								uint64_t uv        = ::std::bit_cast<uint64_t>(value);
								uint64_t sf        = uv >> (sizeof(uint64_t) * 8 - 1);
								uint32_t flip_mask = 0x8000000000000000 | (0xffffffffffffffff * sf);
								return uv ^ flip_mask;
							},
							(sizeof(key_type) * 8) - 8);
		} else {
			static_assert(false, "extract_key must return either a float or a double, long double is not supported!");
		}
	}
#endif

	template<typename It, typename ExtractKey, typename... Args, size_t... Idxs>
	release_force_inline constexpr void counting_sort_tuple_impl(
					It start, It end, ExtractKey extract_key, std::index_sequence<Idxs...>, std::tuple<Args...>&)
	{
		if constexpr (sizeof...(Args) > 1 && (sizeof(Args) + ... + 0) <= 8) {
			// merge into single extract call
			counting_sort_integral(start, end, [](const auto& val) {
				if constexpr ((sizeof(Args) + ... + 0) > 4) {
					const std::tuple<Args...> t = ExtractKey{}(val);
					size_t                    ret;
					size_t                    idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				} else if ((sizeof(Args) + ... + 0) > 2) {
					const std::tuple<Args...> t = ExtractKey{}(val);
					uint32_t                  ret;
					size_t                    idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				} else if ((sizeof(Args) + ... + 0) > 1) {
					const std::tuple<Args...> t = ExtractKey{}(val);
					uint16_t                  ret;
					size_t                    idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				} else {
					const std::tuple<Args...> t = ExtractKey{}(val);
					uint8_t                   ret;
					size_t                    idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				}
			});
		} else {
			((counting_sort_integral(start, end, [](const auto& val) { return ::std::get<Idxs>(ExtractKey{}(val)); })),
							...);
		}
	}

	template<typename> struct is_convertible_to_integrals : std::false_type {};

	template<typename T, typename... Ts>
	struct is_convertible_to_integrals<std::tuple<T, Ts...>>
		: std::bool_constant<(
						  (is_convertible_to_integrals<Ts>::value) && ... && is_convertible_to_integrals<T>::value)> {};

	template<typename It, typename ExtractKey>
	constexpr void counting_sort_tuple(It start, It end, ExtractKey extract_key)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(*std::declval<It>()))>;
		static_assert(is_tuple<key_type>::value && is_convertible_to_integrals<key_type>,
						"extract_key must return a tuple type!");

		counting_sort_get_impl(start, end, extract_key, std::make_index_sequence<std::tuple_size<key_type>::value>{});
	}

	template<typename It, typename ExtractKey = sort::identity_less_than<>>
	constexpr void counting_sort(It start, It end, ExtractKey extract_key)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(::std::move(*std::declval<It>())))>;

		if constexpr (is_tuple<key_type>::value) {
			auto f = get_unwrapped(start);
			auto l = get_unwrapped(end);
			sort::counting_sort_get_impl(
							f, l, extract_key, std::make_index_sequence<std::tuple_size<key_type>::value>{});
		} else if constexpr (::std::is_same<key_type, bool>::value) {
			// partition puts things that return true first...but counting sort should treat this as a value so...we'll
			// flip the extract function to keep the semantics the same as expected
			auto f = get_unwrapped(start);
			auto l = get_unwrapped(end);
			sort::partition(f, l, [](const auto& value) { return !ExtractKey{}(value); });
		} else if constexpr (::std::is_integral<key_type>::value) {
			auto f = get_unwrapped(start);
			auto l = get_unwrapped(end);
			if constexpr (std::is_same<typename ::std::iterator_traits<It>::iterator_category,
										  ::std::random_access_iterator_tag>::value) {
				auto item_count = l - f;
				if (item_count <= insertion_sort_threshold) {
					if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
									::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
						return sort::insertion_sort(f, l, [](const auto& lhs, const auto& rhs) { return lhs < rhs; });
					} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
										 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
						return sort::insertion_sort(f, l, [](const auto& lhs, const auto& rhs) { return lhs > rhs; });
					} else {
						return sort::insertion_sort(f, l, [](const auto& lhs, const auto& rhs) {
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
					}
				}

				if (item_count <= intro_sort_threshold) {
					if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
									::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
						sort::make_heap(f, l, [](const auto& lhs, const auto& rhs) { return lhs < rhs; });
						return sort::sort_heap(f, l, [](const auto& lhs, const auto& rhs) { return lhs < rhs; });
					} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
										 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
						sort::make_heap(f, l, [](const auto& lhs, const auto& rhs) { return lhs > rhs; });
						return sort::sort_heap(f, l, [](const auto& lhs, const auto& rhs) { return lhs > rhs; });
					} else {
						sort::make_heap(f, l, [](const auto& lhs, const auto& rhs) {
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
						return sort::sort_heap(f, l, [](const auto& lhs, const auto& rhs) {
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
					}
				}
			}

			if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
							::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
				if constexpr (::std::is_signed<key_type>::value) {
					// ::std::numeric_limits<key_type>::min();
					constexpr typename ::std::make_unsigned<key_type>::type min_value = {
									~(~typename ::std::make_unsigned<key_type>::type{0} >> 1)};
					sort::counting_sort_byte_shift_flat(f, l, [](const auto& value) {
						return (typename ::std::make_unsigned<key_type>::type)value + min_value;
					});
				} else {
					sort::counting_sort_byte_shift_flat(f, l, extract_key);
				}
			} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
								 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
				if constexpr (::std::is_signed<key_type>::value) {
					constexpr typename ::std::make_unsigned<key_type>::type min_value = {
									~(~typename ::std::make_unsigned<key_type>::type{0} >> 1)};
					sort::counting_sort_byte_shift_flat(f, l, [](const key_type& value) {
						return (typename ::std::make_unsigned<key_type>::type) ~(value + min_value);
					});
				} else {
					sort::counting_sort_byte_shift_flat(f, l, [](const key_type& value) { return ~value; });
				}
			} else {
				sort::counting_sort_byte_shift_flat(f, l, extract_key);
			}
		} else {
			static_assert(false, "counting sort requires some form of integral or boolean convertible type!");
		}
	}

	template<class RAIt, class Compare> constexpr void insertion_sort(RAIt first, RAIt last, Compare comp = Compare{})
	{
		auto i = first;
		for (; i != last; ++i) {
			auto j = i;
			for (; j != first;) {
				auto& rhs = *j;
				auto& lhs = *(--j);
				if (!comp(rhs, lhs))
					break;
				swap_branchless_unconditional(rhs, lhs);
			}
		}
	}

	template<class It, class T, class Comp>
	constexpr void push_heap_by_index(
					It first, iter_difference_t<It> hole, iter_difference_t<It> top, T&& val, Comp comp)
	{
		// percolate hole to top or where val belongs
		using diff = iter_difference_t<It>;
		for (diff idx                                                = (hole - 1) >> 1;   // shift for codegen
						top < hole && comp(*(first + idx), val); idx = (hole - 1) >> 1) { // shift for codegen
			// move hole up to parent
			*(first + hole) = std::move(*(first + idx));
			hole            = idx;
		}

		*(first + hole) = std::forward<T>(val); // drop _Val into final hole
	}

	template<class It, class T, class Comp = std::less<>>
	constexpr void pop_heap_hole_by_index(It first, sort::iter_difference_t<It> hole,
					sort::iter_difference_t<It> bottom, T&& val, Comp comp = Comp{})
	{
		// percolate hole to bottom, then push val
		//_STL_INTERNAL_CHECK(bottom > 0);

		using diff     = sort::iter_difference_t<It>;
		const diff top = hole;
		diff       idx = hole;

		// Check whether idx can have a child before calculating that child's index, since
		// calculating the child's index can trigger integer overflows
		const diff max_sequence_non_leaf = (bottom - 1) >> 1; // shift for codegen
		while (idx < max_sequence_non_leaf) {                 // move hole down to larger child
			idx = 2 * idx + 2;
			if (comp(*(first + idx), *(first + (idx - 1)))) {

				--idx;
			}
			*(first + hole) = std::move(*(first + idx));
			hole            = idx;
		}

		if (idx == max_sequence_non_leaf && bottom % 2 == 0) { // only child at bottom, move hole down to it
			*(first + hole) = std::move(*(first + (bottom - 1)));
			hole            = bottom - 1;
		}

		push_heap_by_index(first, hole, top, std::forward<T>(val), comp);
	}

	template<typename It, typename Comp> constexpr void make_heap(It start, It end, Comp comp = Comp{})
	{
		using diff  = typename sort::iter_difference_t<It>;
		diff bottom = end - start;
		for (diff hole = bottom >> 1; hole > 0;) {
			--hole;
			sort::iter_value_t<It> tmp(std::move(*(start + hole)));
			pop_heap_hole_by_index(start, hole, bottom, ::std::move(tmp), comp);
		}
	}

	template<class It, class T, class Unary>
	constexpr void pop_heap_hole_unchecked(It start, It end, It dest, T&& val, Unary predicate)
	{
		// pop *start to *dest and reheap
		// precondition: start != end
		// precondition: start != dest
		*dest      = std::move(*start);
		using diff = typename sort::iter_difference_t<It>;
		sort::pop_heap_hole_by_index(
						start, static_cast<diff>(0), static_cast<diff>(end - start), ::std::forward<T>(val), predicate);
	}

	template<class It, class Comp = std::less<>> constexpr void pop_heap_unchecked(It start, It end, Comp comp = Comp{})
	{
		// pop *start to *(end - 1) and reheap
		if (2 <= end - start) {
			--end;
			// decltype(*end)
			typename sort::iter_value_t<It> val(std::move(*end));
			sort::pop_heap_hole_unchecked(start, end, end, std::move(val), comp);
		}
	}

	template<typename It, typename Comp> constexpr void sort_heap(It start, It end, Comp comp = Comp{})
	{
		for (; end - start >= 2; --end) {
			sort::pop_heap_unchecked(start, end, comp);
		}
	}

	template<class _RanIt, class _Pr>
	release_force_inline constexpr void med3_unchecked(_RanIt _First, _RanIt _Mid, _RanIt _Last, _Pr _Pred)
	{
		// sort median of three elements to middle
		if (_Pred(*_Mid, *_First)) {
			sort::swap_branchless_unconditional(*_Mid, *_First);
		}

		if (_Pred(*_Last, *_Mid)) { // swap middle and last, then test first again
			sort::swap_branchless_unconditional(*_Last, *_Mid);

			if (_Pred(*_Mid, *_First)) {
				sort::swap_branchless_unconditional(*_Mid, *_First);
			}
		}
	}

	template<class _RanIt, class _Pr>
	release_force_inline constexpr void guess_median_unchecked(_RanIt _First, _RanIt _Mid, _RanIt _Last, _Pr _Pred)
	{
		// sort median element to middle
		using _Diff        = iter_difference_t<_RanIt>;
		const _Diff _Count = _Last - _First;
		if (40 < _Count) {                             // Tukey's ninther
			const _Diff _Step     = (_Count + 1) >> 3; // +1 can't overflow because range was made inclusive in caller
			const _Diff _Two_step = _Step << 1;        // note: intentionally discards low-order bit
			sort::med3_unchecked(_First, _First + _Step, _First + _Two_step, _Pred);
			sort::med3_unchecked(_Mid - _Step, _Mid, _Mid + _Step, _Pred);
			sort::med3_unchecked(_Last - _Two_step, _Last - _Step, _Last, _Pred);
			sort::med3_unchecked(_First + _Step, _Mid, _Last - _Step, _Pred);
		} else {
			sort::med3_unchecked(_First, _Mid, _Last, _Pred);
		}
	}

	template<class It, class Unary>
	constexpr std::pair<It, It> partition_by_median_guess_unchecked(It start, It end, Unary predicate)
	{
		// partition [_First, _Last)
		It _Mid = start + ((end - start) >> 1); // shift for codegen
		sort::guess_median_unchecked(start, _Mid, sort::prev_iter(end), predicate);
		It _Pfirst = _Mid;
		It _Plast  = sort::next_iter(_Pfirst);

		while (start < _Pfirst && !predicate(*sort::prev_iter(_Pfirst), *_Pfirst) &&
						!predicate(*_Pfirst, *sort::prev_iter(_Pfirst))) {
			--_Pfirst;
		}

		while (_Plast < end && !predicate(*_Plast, *_Pfirst) && !predicate(*_Pfirst, *_Plast)) {
			++_Plast;
		}

		It _Gfirst = _Plast;
		It _Glast  = _Pfirst;

		for (;;) { // partition
			for (; _Gfirst < end; ++_Gfirst) {
				if (predicate(*_Pfirst, *_Gfirst)) {
					continue;
				} else if (predicate(*_Gfirst, *_Pfirst)) {
					break;
				} else if (_Plast != _Gfirst) {
					sort::swap_branchless_unconditional(*_Plast, *_Gfirst);
					++_Plast;
				} else {
					++_Plast;
				}
			}

			for (; start < _Glast; --_Glast) {
				const auto _Glast_prev = prev_iter(_Glast);
				if (predicate(*_Glast_prev, *_Pfirst)) {
					continue;
				} else if (predicate(*_Pfirst, *_Glast_prev)) {
					break;
				} else if (--_Pfirst != _Glast_prev) {
					sort::swap_branchless_unconditional(*_Pfirst, *_Glast_prev);
				}
			}

			if (_Glast == start && _Gfirst == end) {
				return std::pair<It, It>(_Pfirst, _Plast);
			}

			if (_Glast == start) { // no room at bottom, rotate pivot upward
				if (_Plast != _Gfirst) {
					sort::swap_branchless_unconditional(*_Pfirst, *_Plast);
				}

				++_Plast;
				sort::swap_branchless_unconditional(*_Pfirst, *_Gfirst);
				++_Pfirst;
				++_Gfirst;
			} else if (_Gfirst == end) { // no room at top, rotate pivot downward
				if (--_Glast != --_Pfirst) {
					sort::swap_branchless_unconditional(*_Glast, *_Pfirst);
				}

				sort::swap_branchless_unconditional(*_Pfirst, *--_Plast);
			} else {
				sort::swap_branchless_unconditional(*_Gfirst, *--_Glast);
				++_Gfirst;
			}
		}
	}

	template<typename It, typename Compare>
	constexpr void intro_sort(It first, It last, Compare comp = Compare{}, size_t heapthresh = ~size_t{0})
	{
		for (;;) {
			size_t count = sort::distance(first, last);

			if (count <= 32) {
				sort::insertion_sort(first, last, comp);
				return;
			}

			if (heapthresh <= 0) {
				sort::make_heap(first, last, comp);
				sort::sort_heap(first, last, comp);
				return;
			}

			auto mid   = sort::partition_by_median_guess_unchecked(first, last, comp);
			heapthresh = (heapthresh >> 1) + (heapthresh >> 2);

			if (mid.first - first < last - mid.second) {
				sort::intro_sort(first, mid.first, comp, heapthresh);
				first = mid.second;
			} else {
				sort::intro_sort(mid.second, last, comp, heapthresh);
				last = mid.first;
			}
		}
	}

	enum sorting_algorithms {
		s_partition,
		s_bubble_sort,
		s_selection_sort,
		s_insertion_sort,
		s_merge_sort,
		s_heap_sort,
		s_quick_sort,
		s_intro_sort,
		s_counting_sort,
		s_radix_sort
	};

	/* Returns the sorting algorithm used given parameters that would be passed to sort */
	template<typename It, typename Comp = std::less<>> constexpr sorting_algorithms sorting_algorithm(It, It, Comp)
	{
		using value_type         = decltype(*std::declval<It>());
		using comparator_details = comparator_info<It, Comp>;

		if constexpr (comparator_details::is_partition) {
			return s_partition;
		} else if constexpr (std::is_integral<value_type>::value &&
							 (std::is_same<Comp, std::less<>>::value ||
											 std::is_same<Comp, std::less<value_type>>::value)) {
			return s_counting_sort;
		} else if constexpr (std::is_integral<value_type>::value &&
							 (std::is_same<Comp, std::greater<>>::value ||
											 std::is_same<Comp, std::greater<value_type>>::value)) {
			return s_counting_sort;
		} else if constexpr (comparator_details::is_keyed) {
			return s_counting_sort;
		} else {
			return s_intro_sort;
		}
	}

	template<typename It, typename Comp = std::less<>> constexpr void sort(It start, It end, Comp comp = Comp{})
	{
		using value_type         = sort::remove_cvref_t<decltype(*std::declval<It>())>;
		using comparator_details = comparator_info<It, Comp>;

		if constexpr (comparator_details::is_partition) {
			// partition puts things that return true first...but counting sort should treat this as a value
			// so...we'll flip the extract function to keep the semantics the same as expected
			sort::reversed_partition(get_unwrapped(start), get_unwrapped(end), comp);
		}
		// we're sorting integral data using < or >, use counting sort
		else if constexpr (std::is_integral<value_type>::value &&
						   (std::is_same<Comp, std::less<>>::value ||
										   std::is_same<Comp, std::less<value_type>>::value)) {
			sort::counting_sort(start, end, identity_less_than<value_type>{});
		} else if constexpr (std::is_integral<value_type>::value &&
							 (std::is_same<Comp, std::greater<>>::value ||
											 std::is_same<Comp, std::greater<value_type>>::value)) {
			sort::counting_sort(start, end, identity_greater_than<value_type>{});
		} else if constexpr (comparator_details::is_keyed) {
			sort::counting_sort(start, end, comp);
		} else {
			sort::intro_sort(get_unwrapped(start), get_unwrapped(end), comp, end - start);
		}
	}
} // namespace sort