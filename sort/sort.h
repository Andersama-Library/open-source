#pragma once
#include <cstdint>
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

namespace sort {
	template<typename It, typename Comp> struct comparator_info {
		using comparator_value_type  = decltype(*std::declval<It>());
		constexpr bool is_comparator = ::std::is_invocable<Comp, comparator_value_type, comparator_value_type>::value;
		constexpr bool is_keyed      = ::std::is_invocable<Comp, comparator_value_type>::value;
		constexpr bool is_partition  = ::std::is_invocable<bool, Comp, comparator_value_type>::value;
	};

	template<class ForwardIt1, class ForwardIt2> constexpr void iter_swap(ForwardIt1 a, ForwardIt2 b)
	{
		using value_type  = typename std::iterator_traits<ForwardIt1>::value_type;
		using value_type2 = typename std::iterator_traits<ForwardIt2>::value_type;
		if constexpr (::std::is_trivial<value_type>::value) {
			auto temp = *a;
			*a        = *b;
			*b        = temp;
		} else if (::std::is_invocable<decltype(a->swap), value_type2 &>::value) {
			*a.swap(*b);
		} else if (::std::is_swappable_with<value_type, value_type2>::value) {
			using std::swap;
			swap(*a, *b);
		} else {
			static_assert(false, "iter swap requires that the dereferenced types are swappable!");
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
				iter_swap(i, first);
				++first;
			}
		}

		return first;
	}

	// https://github.com/skarupke/ska_sort/blob/master/ska_sort.hpp
	template<typename It, typename ExtractKey>
	release_force_inline constexpr void counting_sort_byte(It start, It end, ExtractKey extract_key)
	{
		// using count_type = ::std::iterator_traits<It>::value_type;
		using key_type = decltype(ExtractKey{}(*std::declval<It>()));
		static_assert(std::is_integral<key_type>::value, "extract_key must return an integral type!");

		size_t counts[256] = {0};
		size_t starts[256];
		size_t ends[256];

		for (It it = start; it != end; ++it) {
			++counts[extract_key(*it)];
		}
		size_t total = 0;
		// convert our counts to starting indexs
		// IE, if we know we've seen 4 of some value
		// then the current total is the index these values
		// would start at and we know the total would increase by 4
		{
			size_t idx = 0;
			for (size_t &count : counts) {
				size_t old_count = count;
				count            = total;
				starts[idx]      = total;
				ends[idx]        = total + old_count;
				total += old_count;
				idx++;
			}
		}

		It     begin = start;
		size_t idx   = 0;
		for (; begin != end;) {
			uint8_t key        = extract_key(*begin);
			size_t  target_idx = counts[key];
			size_t  start_idx  = starts[key];
			size_t  end_idx    = ends[key];
			if (idx >= start_idx && idx < target_idx) {
				// the data here is already in place
				++start;
				idx++;
			} else {
				It target = start + target_idx;
				counts[key] += 1;
				std::swap(*start, *target);
				swapped = true;
			}
		}
	}

	template<typename It, typename ExtractKey>
	release_force_inline constexpr void counting_sort_integral(It start, It end, ExtractKey extract_key)
	{
		using key_type = decltype(ExtractKey{}(*std::declval<It>()));
		static_assert(std::is_integral<key_type>::value, "extract_key must return an integral type!");

		if constexpr (::std::is_signed<key_type>::value) {
			constexpr key_type r = ::std::numeric_limits<key_type>::min();
			counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r)) & 0xff; });
			if constexpr (sizeof(key_type) >= 2)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 8) & 0xff; });
			if constexpr (sizeof(key_type) >= 3)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 16) & 0xff; });
			if constexpr (sizeof(key_type) >= 4)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 24) & 0xff; });
			if constexpr (sizeof(key_type) >= 5)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 32) & 0xff; });
			if constexpr (sizeof(key_type) >= 6)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 40) & 0xff; });
			if constexpr (sizeof(key_type) >= 7)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 48) & 0xff; });
			if constexpr (sizeof(key_type) >= 8)
				counting_sort_byte(start, end, [](const It &it) { return ((extract_key(*it) + r) >> 56) & 0xff; });
		} else {
			counting_sort_byte(start, end, [](const It &it) { return extract_key(*it) & 0xff; });
			if constexpr (sizeof(key_type) >= 2)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 8) & 0xff; });
			if constexpr (sizeof(key_type) >= 3)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 16) & 0xff; });
			if constexpr (sizeof(key_type) >= 4)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 24) & 0xff; });
			if constexpr (sizeof(key_type) >= 5)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 32) & 0xff; });
			if constexpr (sizeof(key_type) >= 6)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 40) & 0xff; });
			if constexpr (sizeof(key_type) >= 7)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 48) & 0xff; });
			if constexpr (sizeof(key_type) >= 8)
				counting_sort_byte(start, end, [](const It &it) { return (extract_key(*it) >> 56) & 0xff; });
		}
	}

	#if defined(cplusplus_version_20)
	template<typename It, typename ExtractKey>
	constexpr void counting_sort_floating(It start, It end, ExtractKey extract_key)
	{
		using key_type = decltype(ExtractKey{}(*std::declval<It>()));
		static_assert(std::is_floating_point<key_type>::value, "extract_key must return a floating point type!");

		if constexpr (::std::same_as<key_type, float>::value) {
			constexpr key_type r = ::std::numeric_limits<key_type>::min();
			counting_sort_integral(start, end, [](const It &it) {
				const auto v  = extract_key(*it);
				uint32_t   uv = ::std::bit_cast<uint32_t>(v);
				return uv ^ (~uint32_t{0} - (~uint32_t{0} >> 1)); // make unsigned values appear small and signed values
				// big etc...
			});
		} else if (::std::same_as<key_type, double>::value) {
			counting_sort_integral(start, end, [](const It &it) {
				const auto v  = extract_key(*it);
				uint64_t   uv = ::std::bit_cast<uint64_t>(v);
				return uv ^ (~uint64_t{0} - (~uint64_t{0} >> 1)); // make unsigned values appear small and signed values
				// big etc...
			});
		}
	}
	#endif

	template<typename It, typename ExtractKey, typename... Args, size_t... Idxs>
	release_force_inline constexpr void counting_sort_tuple_impl(
		It start, It end, ExtractKey extract_key, std::index_sequence<Idxs...>, std::tuple<Args...> &)
	{
		if constexpr (sizeof...(Args) > 1 && (sizeof(Args) + ... + 0) <= 8) {
			// merge into single extract call
			counting_sort_integral(start, end, [](const auto &val) {
				if constexpr ((sizeof(Args) + ... + 0) > 4) {
					const std::tuple<Args...> t = ExtractKey{}(val));
				size_t                    ret;
				size_t                    idx = 0;
				((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
				return ret;
				} else if ((sizeof(Args) + ... + 0) > 2) {
					uint32_t ret;
					size_t   idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				} else if ((sizeof(Args) + ... + 0) > 1) {
					uint16_t ret;
					size_t   idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				} else {
					uint8_t ret;
					size_t  idx = 0;
					((ret |= ::std::get<Idxs>(t) << idx, idx += sizeof(Args) * 8), ...);
					return ret;
				}
			});
		} else {
			((counting_sort_integral(start, end, [](const auto &val) { return ::std::get<Idxs>(ExtractKey{}(val)); })),
				...);
		}
	}

	template<typename> struct is_tuple : std::false_type {};

	template<typename... T> struct is_tuple<std::tuple<T...>> : std::true_type {};

	template<typename T> struct is_convertible_to_integrals<T> : std::is_convertible<T, size_t> {};

	template<typename... T>
	struct is_convertible_to_integrals<std::tuple<T...>>
	: std::bool_constant<is_convertible_to_integrals<T> &&... && true> {};

	template<typename It, typename ExtractKey>
	constexpr void counting_sort_tuple(It start, It end, ExtractKey extract_key)
	{
		using key_type = decltype(ExtractKey{}(*std::declval<It>()));
		static_assert(is_tuple<key_type>::value && is_convertible_to_integrals<key_type>,
			"extract_key must return a tuple type!");

		counting_sort_tuple_impl(start, end, extract_key, std::make_index_sequence<std::tuple_size<key_type>::value>{},
		key_type{});
	}

	template<typename It, typename ExtractKey> constexpr void counting_sort(It start, It end, ExtractKey extract_key)
	{
		using key_type = decltype(ExtractKey{}(*std::declval<It>()));
		if constexpr (is_tuple<key_type>::value)
			counting_sort_tuple_impl(start, end, extract_key,
									std::make_index_sequence<std::tuple_size<key_type>::value>{}, key_type{});
		else if (::std::is_same<key_type, bool>::value)
			// partition puts things that return true first...but counting sort should treat this as a value so...we'll
			// flip the extract function to keep the semantics the same as expected
			partition(start, end, [](const auto &value) { return !ExtractKey{}(value); });
		else if (::std::is_integral<key_type>::value)
			counting_sort_integral(start, end, extract_key);
		else
			static_assert(false, "counting sort requires some form of integral or boolean convertible type!")
	}

	template<typename T> release_force_inline constexpr void swap_branchless_unconditional(T &lhs, T &rhs)
	{
		if constexpr (::std::is_trivial<T>::value) {
			auto temp = *a;
			*a        = *b;
			*b        = temp;
		} else if (::std::is_invocable<decltype(a.swap), T &>::value) {
			*a.swap(*b);
		} else if (::std::is_swappable_with<T, T>::value) {
			using std::swap;
			swap(*a, *b);
		} else {
			static_assert(false, "swap_branchless_unconditional requires that the types are swappable!");
		}
	}

	template<class RAIt, class Compare = std::less<>>
	constexpr void insertion_sort(RAIt first, RAIt last, Compare comp = Compare{})
	{
		size_t dist = last - first;
		for (size_t i = 1; i < dist; i++) {
			size_t j = i;
			for (; j && comp(first[j], first[j - 1]); j--) {
				swap_branchless_unconditional(first[j], first[j - 1]);
			}
		}
	}

	#if defined(cplusplus_version_20)
	template<class T1, class T2>
	constexpr bool same_impl = // Must be a distinct concept to provide symmetric subsumption for same_as
	#ifdef __clang__
	__is_same(T1, T2);
	#else  // ^^^ use intrinsic / no intrinsic vvv
	std::is_same_v<T1, T2>;
	#endif // ^^^ no intrinsic ^^^

	template<class _Ty> constexpr bool is_specialized = same_impl<typename _Ty::_From_primary, _Ty>;

	template<class T>
	using iter_difference_t =
	std::conditional_t<is_specialized<std::iterator_traits<std::remove_cvref_t<T>>>, // supposed to be a
	// sfinae check
	std::incrementable_traits<std::remove_cvref_t<T>>,
	std::iterator_traits<std::remove_cvref_t<T>>>::difference_type;

	template<class T>
	using iter_value_t =
	std::conditional_t<is_specialized<std::iterator_traits<std::remove_cvref_t<T>>>, // supposed to be a
	// sfinae check
	std::incrementable_traits<std::remove_cvref_t<T>>,
	std::iterator_traits<std::remove_cvref_t<T>>>::value_type;
	#else // defined(cplusplus_version_17) || defined(cplusplus_version_14)
	template<class T> using remove_cvref_t = typename std::remove_cv_t<std::remove_reference_t<T>>;

	template<class T> using iter_difference_t = typename std::iterator_traits<sort::remove_cvref_t<T>>::difference_type;

	template<class T> using iter_value_t = typename std::iterator_traits<sort::remove_cvref_t<T>>::value_type;
	#endif

	template<class It, class T, class Comp>
	constexpr void push_heap_by_index(
		It first, iter_difference_t<It> hole, iter_difference_t<It> top, T &&val, Comp comp)
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
	constexpr void pop_heap_hole_by_index(
		It first, sort::iter_difference_t<It> hole, sort::iter_difference_t<It> bottom, T &&val, Comp comp = Comp{})
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

		if (_Idx == max_sequence_non_leaf && bottom % 2 == 0) { // only child at bottom, move hole down to it
			*(first + hole) = std::move(*(first + (bottom - 1)));
			hole            = bottom - 1;
		}

		push_heap_by_index(first, hole, top, std::forward<T>(val), comp);
	}

	template<typename It, typename Comp = std::less<>> constexpr void make_heap(It start, It end, Comp comp = Comp{})
	{
		using diff  = sort::iter_difference_t<It>;
		diff bottom = end - start;
		for (diff hole = bottom >> 1; hole > 0;) {
			--hole;
			sort::iter_value_t<It> tmp(std::move(*(start + hole)));
			pop_heap_hole_by_index(first, hole, bottom, ::std::move(tmp), comp);
		}
	}

	template<class It, class Comp = std::less<>> constexpr void pop_heap_unchecked(It start, It end, Comp comp = Comp{})
	{
		// pop *start to *(end - 1) and reheap
		if (2 <= end - start) {
			--end;
			decltype(*end) val(std::move(*end));
			pop_heap_hole_unchecked(start, end, end, std::move(val), comp);
		}
	}

	template<typename It, typename Comp = std::less<>> constexpr void sort_heap(It start, It end, Comp comp = Comp{})
	{
		for (; end - start >= 2; --end) {
			pop_heap_unchecked(start, end, comp);
		}
	}

	template<class _RanIt, class _Pr>
	release_force_inline constexpr void med3_unchecked(_RanIt _First, _RanIt _Mid, _RanIt _Last, _Pr _Pred)
	{
		// sort median of three elements to middle
		if (_Pred(*_Mid, *_First)) {
			swap_branchless_unconditional(*_Mid, *_First); // intentional ADL
		}

		if (_Pred(*_Last, *_Mid)) {                       // swap middle and last, then test first again
			swap_branchless_unconditional(*_Last, *_Mid); // intentional ADL

			if (_Pred(*_Mid, *_First)) {
				swap_branchless_unconditional(*_Mid, *_First); // intentional ADL
			}
		}
	}

	template<class _RanIt, class _Pr>
	release_force_inline constexpr void _Guess_median_unchecked(_RanIt _First, _RanIt _Mid, _RanIt _Last, _Pr _Pred)
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

	template<typename It> constexpr It prev_iter(It it)
	{
		return --it;
	}

	template<typename It> constexpr It next_iter(It it)
	{
		return ++it;
	}

	template<class _RanIt, class _Pr>
	constexpr std::pair<_RanIt, _RanIt> partition_by_median_guess_unchecked(_RanIt _First, _RanIt _Last, _Pr _Pred)
	{
		// partition [_First, _Last)
		_RanIt _Mid = _First + ((_Last - _First) >> 1); // shift for codegen
		_Guess_median_unchecked(_First, _Mid, prev_iter(_Last), _Pred);
		_RanIt _Pfirst = _Mid;
		_RanIt _Plast  = next_iter(_Pfirst);

		while (_First < _Pfirst && !_Pred(*prev_iter(_Pfirst), *_Pfirst) && !_Pred(*_Pfirst, *prev_iter(_Pfirst))) {
			--_Pfirst;
		}

		while (_Plast < _Last && !_Pred(*_Plast, *_Pfirst) && !_Pred(*_Pfirst, *_Plast)) {
			++_Plast;
		}

		_RanIt _Gfirst = _Plast;
		_RanIt _Glast  = _Pfirst;

		for (;;) { // partition
			for (; _Gfirst < _Last; ++_Gfirst) {
				if (_Pred(*_Pfirst, *_Gfirst)) {
					continue;
				} else if (_Pred(*_Gfirst, *_Pfirst)) {
					break;
				} else if (_Plast != _Gfirst) {
					swap_branchless_unconditional(*_Plast, *_Gfirst); // intentional ADL
					++_Plast;
				} else {
					++_Plast;
				}
			}

			for (; _First < _Glast; --_Glast) {
				const auto _Glast_prev = prev_iter(_Glast);
				if (_Pred(*_Glast_prev, *_Pfirst)) {
					continue;
				} else if (_Pred(*_Pfirst, *_Glast_prev)) {
					break;
				} else if (--_Pfirst != _Glast_prev) {
					swap_branchless_unconditional(*_Pfirst, *_Glast_prev); // intentional ADL
				}
			}

			if (_Glast == _First && _Gfirst == _Last) {
				return std::pair<_RanIt, _RanIt>(_Pfirst, _Plast);
			}

			if (_Glast == _First) { // no room at bottom, rotate pivot upward
				if (_Plast != _Gfirst) {
					swap_branchless_unconditional(*_Pfirst, *_Plast); // intentional ADL
				}

				++_Plast;
				swap_branchless_unconditional(*_Pfirst, *_Gfirst); // intentional ADL
				++_Pfirst;
				++_Gfirst;
			} else if (_Gfirst == _Last) { // no room at top, rotate pivot downward
				if (--_Glast != --_Pfirst) {
					swap_branchless_unconditional(*_Glast, *_Pfirst); // intentional ADL
				}

				swap_branchless_unconditional(*_Pfirst, *--_Plast); // intentional ADL
			} else {
				swap_branchless_unconditional(*_Gfirst, *--_Glast); // intentional ADL
				++_Gfirst;
			}
		}
	}

	template<typename It, typename Compare>
	constexpr void intro_sort(It first, It last, Compare comp = Compare{}, size_t heapthresh = ~size_t{0})
	{
		for (;;) {
			size_t count = ::std::distance(first, last);

			if (count <= 32) {
				insertion_sort(first, last, comp);
				return;
			}

			if (heapthresh > 0) {
				make_heap(first, last, comp);
				sort_heap(first, last, comp);
				return;
			}

			auto mid   = parition_by_median_guess_unchecked(first, last, comp);
			heapthresh = (heapthresh >> 1) + (heapthresh >> 2);

			if (mid.first - first < last - mid.second) {
				intro_sort(first, mid.first, comp, heapthresh);
				first = mid.second;
			} else {
				intro_sort(mid.second, last, comp, heapthresh);
				last = mid.first;
			}
		}
	}

	template<typename It, typename Comp = std::less<>> constexpr void sort(It start, It end, Comp comp = Comp{})
	{
		using value_type         = decltype(*std::declval<It>());
		using comparator_details = comparator_info<It, Comp>;

		if constexpr (comparator_details::is_partition) {
			// partition puts things that return true first...but counting sort should treat this as a value so...we'll
			// flip the extract function to keep the semantics the same as expected
			partition(start, end, [](const auto &value) { return !Comp{}(value); });
		} else if constexpr (std::is_integral<value_type>::value && std::is_same<Comp, std::less<>>) {
			counting_sort(start, end, [](const value_type v) { return v; });
		} else if constexpr (std::is_integral<value_type>::value && std::is_same<Comp, std::greater<>>) {
			counting_sort(start, end, [](const value_type v) { return ~value_type{0} - v; });
			// counting_sort(start, end, [](const value_type v) { return v; });
		} else if (comparator_details::is_keyed) {
			counting_sort(start, end, comp);
		} else {
			// TODO:
		}
	}
}