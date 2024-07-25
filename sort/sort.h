#pragma once
#include <cstdint>
#include <array>
#include <tuple>
#include <type_traits>
#include <functional>

#if defined __has_include
#if __has_include(<bitset>)
#include <bitset>
#endif
#endif

#if defined __has_include
#if __has_include(<optional>)
#include <optional>
#endif
#endif

/* TODO:
#if defined __has_include
#if __has_include(<variant>)
#include <variant>
#endif
#endif
*/

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
	template<class T> using remove_cvref_t = typename std::remove_cv_t<std::remove_reference_t<T>>;
#if defined(cplusplus_version_20)
	template<class T> using iter_difference_t = ::std::iter_difference_t<T>;

	template<class T> using iter_value_t = ::std::iter_value_t<T>;
#else // defined(cplusplus_version_17) || defined(cplusplus_version_14)
	template<class T> using iter_difference_t = typename std::iterator_traits<sort::remove_cvref_t<T>>::difference_type;

	template<class T> using iter_value_t = typename std::iterator_traits<sort::remove_cvref_t<T>>::value_type;
#endif

	// Detect forward iterators vs random access iterators
	template<typename, typename = std::void_t<>> struct has_pre_increment_member : std::false_type {};

	template<typename T>
	struct has_pre_increment_member<T, std::void_t<decltype(++std::declval<T&>())>> : std::true_type {};

	template<typename, typename = std::void_t<>> struct has_assign_addition_member : std::false_type {};

	template<typename T>
	struct has_assign_addition_member<T, std::void_t<decltype(std::declval<T&>() += std::declval<size_t>())>>
		: std::true_type {};

	template<typename, typename = std::void_t<>> struct has_addition_member : std::false_type {};

	template<typename T>
	struct has_addition_member<T, std::void_t<decltype(std::declval<T&>() + std::declval<size_t>())>> : std::true_type {
	};

	template<class, class = std::void_t<>> struct has_type_member : std::false_type {};
	template<class T> struct has_type_member<T, std::void_t<typename T::type>> : std::true_type {};

	template<typename It, typename Comp, bool is_comparator> constexpr auto comparator_callback_type()
	{
		using comparator_value_type = sort::remove_cvref_t<decltype(*std::declval<It>())>;
		// using if constexpr to prevent the evaluation of std::invoke_result_t with types that don't work
		if constexpr (is_comparator) {
			return std::invoke_result_t<Comp, comparator_value_type, comparator_value_type>{};
		} else {
			return std::invoke_result_t<Comp, comparator_value_type>{};
		}
	}

	template<typename It, typename Comp> struct comparator_info {
		using comparator_value_type = sort::remove_cvref_t<decltype(*std::declval<It>())>;

		static constexpr bool is_comparator =
						::std::is_invocable<Comp, comparator_value_type, comparator_value_type>::value;
		static constexpr bool is_keyed = ::std::is_invocable<Comp, comparator_value_type>::value;
		static_assert(is_comparator || is_keyed, "The provided callback requires one or two arguments");

		using result_type = decltype(sort::comparator_callback_type<It, Comp, is_comparator>());

		static constexpr bool is_boolean_comparator = is_comparator && std::is_same<comparator_value_type, bool>::value;
		static constexpr bool is_partition          = is_keyed && std::is_same<result_type, bool>::value;
	};

	constexpr size_t insertion_sort_threshold   = 32;
	constexpr size_t intro_sort_threshold       = 128;
	constexpr size_t small_merge_sort_threshold = 128; // must be strictly < 256

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

	template<typename> struct is_array : std::false_type {};
	template<typename T, size_t N> struct is_array<std::array<T, N>> : std::true_type {};

#if defined __has_include
#if __has_include(<bitset>)
	template<typename> struct is_bitset : std::false_type {};
	template<size_t N> struct is_bitset<std::bitset<N>> : std::true_type {};
#endif
#endif

#if defined __has_include
#if __has_include(<optional>)
	template<typename> struct is_optional : std::false_type {};
	template<typename T> struct is_optional<std::optional<T>> : std::true_type {};
#endif
#endif

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

	template<class T = void> struct less {
		[[nodiscard]] constexpr bool operator()(const T& lhs, const T& rhs) const
		// noexcept(noexcept(_STD _Fake_copy_init<bool>(_Left < _Right))) /* strengthened */
		{
			using namespace sort;
			return lhs < rhs;
		}
	};
	// Wrappers so we can expand > and < support inside namespace sort vs namespace std (which can be UB)
	template<> struct less<void> {
		template<class T0, class T1>
		_NODISCARD constexpr auto operator()(T0&& lhs, T1&& rhs) const
						noexcept(noexcept(static_cast<T0&&>(lhs) < static_cast<T1&&>(rhs))) // strengthened
						-> decltype(static_cast<T0&&>(lhs) < static_cast<T1&&>(rhs))
		{
			using namespace sort;
			return static_cast<T0&&>(lhs) < static_cast<T1&&>(rhs);
		}

		using is_transparent = int;
	};

	template<class T = void> struct greater {
		[[nodiscard]] constexpr bool operator()(const T& lhs, const T& rhs) const
		// noexcept(noexcept(_STD _Fake_copy_init<bool>(_Left < _Right))) /* strengthened */
		{
			using namespace sort;
			return lhs > rhs;
		}
	};

	template<> struct greater<void> {
		template<class T0, class T1>
		_NODISCARD constexpr auto operator()(T0&& lhs, T1&& rhs) const
						noexcept(noexcept(static_cast<T0&&>(lhs) < static_cast<T1&&>(rhs))) // strengthened
						-> decltype(static_cast<T0&&>(lhs) < static_cast<T1&&>(rhs))
		{
			using namespace sort;
			return static_cast<T0&&>(lhs) > static_cast<T1&&>(rhs);
		}

		using is_transparent = int;
	};

	template<typename Callback> struct wrapped_greater_than : Callback {};
	template<typename> struct is_wrapped_greater_than : std::false_type {};
	template<typename Callback> struct is_wrapped_greater_than<wrapped_greater_than<Callback>> : std::true_type {};

	template<typename> struct is_std_less : std::false_type {};
	template<typename T> struct is_std_less<std::less<T>> : std::true_type {};

	template<typename> struct is_std_greater : std::false_type {};
	template<typename T> struct is_std_greater<std::greater<T>> : std::true_type {};

	template<class It> [[nodiscard]] constexpr decltype(auto) get_unwrapped(It&& it)
	{
		if constexpr (::std::is_pointer_v<::std::decay_t<It>>) { // special-case pointers and arrays
			return it + 0;
		} else if constexpr (is_msvc_unwrappable_iterator<It>::value) {
			return static_cast<It&&>(it)._Unwrapped();
		} else if constexpr (is_pointer_convertible_iterator<It>::value) {
			return static_cast<typename ::std::iterator_traits<It>::pointer_type>(it);
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

	template<typename... Ts> struct parameter_list {};

	template<typename T, typename... Ts> constexpr auto front(parameter_list<T, Ts...>) -> T
	{
		return {};
	};

	template<typename T, typename... Ts> constexpr auto pop_front(parameter_list<T, Ts...>) -> parameter_list<Ts...>
	{
		return {};
	};

	constexpr auto pop_front(parameter_list<>) -> parameter_list<>
	{
		return {};
	};

	template<typename... Ts> constexpr auto size(parameter_list<Ts...>) noexcept
	{
		return sizeof...(Ts);
	}

	// check if the parameter_list is empty
	template<typename... Ts> constexpr bool empty(parameter_list<Ts...>) noexcept
	{
		return false;
	}

#if defined __has_include
#if __has_include(<bitset>)
	template<size_t L, size_t R> constexpr bool operator<(const std::bitset<L>& lhs, const std::bitset<R>& rhs)
	{
		if constexpr (R > L) {
			std::bitset<R> tmp = rhs >> L;
			if (tmp.count())
				return true;
		} else if constexpr (L > R) {
			std::bitset<R> tmp = lhs >> R;
			if (tmp.count())
				return false;
		}
		constexpr size_t mn = L < R ? L : R;
		for (size_t i = mn; --i < mn;) {
			if (lhs[i] < rhs[i])
				return true;
		}
		return false;
	}

	template<size_t L, size_t R> constexpr bool operator>(const std::bitset<L>& lhs, const std::bitset<R>& rhs)
	{
		if constexpr (R > L) {
			std::bitset<R> tmp = rhs >> L;
			if (tmp.count())
				return false;
		} else if constexpr (L > R) {
			std::bitset<R> tmp = lhs >> R;
			if (tmp.count())
				return true;
		}
		constexpr size_t mn = L < R ? L : R;
		for (size_t i = mn; --i < mn;) {
			if (lhs[i] > rhs[i])
				return true;
		}
		return false;
	}
#endif
#endif

	template<class It, class Compare = sort::less<>> constexpr void insertion_sort(It, It, Compare comp = Compare{});

	template<typename It, typename Compare = sort::less<>>
	constexpr void intro_sort(It, It, Compare = Compare{}, size_t = ~size_t{0});

	template<typename It, typename Compare = sort::less<>> constexpr void make_heap(It, It, Compare = Compare{});

	template<typename It, typename Compare = sort::less<>> constexpr void sort_heap(It, It, Compare = Compare{});

	template<typename It> constexpr It prev_iter(It it)
	{
		return --it;
	}

	template<typename It> constexpr It next_iter(It it)
	{
		return ++it;
	}

	template<typename T, typename R, typename = void> struct has_swap_member : std::false_type {};

	template<typename T, typename R>
	struct has_swap_member<T, R, std::void_t<decltype(std::declval<T&>().swap(std::declval<R&>()))>> : std::true_type {
	};

	template<class ForwardIt1, class ForwardIt2>
	release_force_inline constexpr void iter_swap(ForwardIt1 a, ForwardIt2 b)
	{
		using value_type  = sort::remove_cvref_t<typename std::iterator_traits<ForwardIt1>::value_type>;
		using value_type2 = sort::remove_cvref_t<typename std::iterator_traits<ForwardIt2>::value_type>;
		if constexpr (has_swap_member<value_type, value_type2>::value) { // use type's provided swap
																		 // function if it exists
			(*a).swap(*b);
		} else if constexpr (::std::is_swappable_with<value_type&, value_type2&>::value ||
							 ::std::is_swappable_with<value_type,
											 value_type2>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			swap(*a, *b);
		} else if constexpr (::std::is_trivial<value_type>::value) {
			auto temp = *a;
			*a        = *b;
			*b        = temp;
		} else {
			static_assert(false, "iter swap requires that the dereferenced types are swappable!");
		}
	}

	template<class ForwardIt1, class ForwardIt2>
	release_force_inline constexpr void iter_swap_conditional(ForwardIt1 a, ForwardIt2 b, bool c)
	{
		using value_type  = sort::remove_cvref_t<typename std::iterator_traits<ForwardIt1>::value_type>;
		using value_type2 = sort::remove_cvref_t<typename std::iterator_traits<ForwardIt2>::value_type>;
		if constexpr (has_swap_member<value_type, value_type2>::value) { // use type's provided swap
																		 // function if it exists
			if (c)
				(*a).swap(*b);
		} else if constexpr (::std::is_swappable_with<value_type&, value_type2&>::value ||
							 ::std::is_swappable_with<value_type,
											 value_type2>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			if (c)
				swap(*a, *b);
		} else if constexpr (::std::is_trivial<value_type>::value) {
			decltype(*a) tmp[2] = {*a, *b};
			*a                  = tmp[c];
			*b                  = tmp[!c];
		} else {
			static_assert(false, "iter swap requires that the dereferenced types are swappable!");
		}
	}

	template<typename T> release_force_inline constexpr void swap_branchless_unconditional(T& lhs, T& rhs)
	{
		if constexpr (has_swap_member<T, T>::value) { // use type's provided swap function if it exists
													  //::std::is_invocable<decltype(&(lhs.swap)), T& > ::value
			lhs.swap(rhs);
		} else if constexpr (::std::is_swappable_with<T&, T&>::value ||
							 ::std::is_swappable_with<T,
											 T>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			swap(lhs, rhs);
		} else if constexpr (::std::is_trivial<T>::value) {
			auto tmp = lhs;
			lhs      = rhs;
			rhs      = tmp;
		} else {
			static_assert(false, "swap_branchless_unconditional requires that the types are swappable!");
		}
	}

	template<typename T> release_force_inline constexpr void swap_branchless_conditional(T& lhs, T& rhs, bool c)
	{
		if constexpr (has_swap_member<T, T>::value) { // use type's provided swap function if it exists
			if (c)
				lhs.swap(rhs);
		} else if constexpr (::std::is_swappable_with<T&, T&>::value ||
							 ::std::is_swappable_with<T,
											 T>::value) { // fallback to std::swap, not necessarily constexpr
			using std::swap;
			if (c)
				swap(lhs, rhs);
		} else if constexpr (::std::is_trivial<T>::value) {
			T tmp[2] = {lhs, rhs};
			lhs      = tmp[c];
			rhs      = tmp[!c];
		} else {
			static_assert(false, "swap_branchless_unconditional requires that the types are swappable!");
		}
	}

	template<typename It, typename It2, typename It3, typename It4, typename ItDest, typename Compare = std::less<>>
	always_force_inline constexpr ItDest merge(It first0, It2 last0, It3 first1, It4 last1, ItDest out, Compare comp)
	{
		using T = sort::iter_value_t<It>;

		if constexpr ((std::is_arithmetic<T>::value || std::is_same<T, bool>::value) &&
						std::is_same<sort::remove_cvref_t<decltype(*first0)>, T>::value) {
			// this second line is to gaurd against proxy references /
			// reference wrappers
			if (first0 != last0 && first1 != last1) {
				for (;;) {
					if (comp(*first1, *first0)) {
						*out = *first1;
						++out;
						++first1;

						if (first1 == last1) {
							break;
						}
					} else {
						*out = *first0;
						++out;
						++first0;

						if (first0 == last0) {
							break;
						}
					}
				}
			}

			for (; first0 != last0;) {
				*out = *first0;
				++out;
				++first0;
			}

			for (; first1 != last1;) {
				*out = *first1;
				++out;
				++first1;
			}
		} else if constexpr (std::is_move_constructible<T>::value) {
			if (first0 != last0 && first1 != last1) {
				for (;;) {
					if (comp(*first1, *first0)) {
						*out = ::std::move(*first1); //
						++out;
						++first1;

						if (first1 == last1) {
							break;
						}
					} else {
						*out = ::std::move(*first0);
						++out;
						++first0;

						if (first0 == last0) {
							break;
						}
					}
				}
			}

			for (; first0 != last0;) {
				*out = ::std::move(*first0);
				++out;
				++first0;
			}

			for (; first1 != last1;) {
				*out = ::std::move(*first1);
				++out;
				++first1;
			}
		} else { // else if constexpr (std::is_assignable<T, T>::value)
			if (first0 != last0 && first1 != last1) {
				for (;;) {
					if (comp(*first1, *first0)) {
						sort::iter_swap(out, first1); //
						++out;
						++first1;

						if (first1 == last1) {
							break;
						}
					} else {
						sort::iter_swap(out, first0);
						++out;
						++first0;

						if (first0 == last0) {
							break;
						}
					}
				}
			}

			for (; first0 != last0;) {
				sort::iter_swap(out, first0);
				++out;
				++first0;
			}

			for (; first1 != last1;) {
				sort::iter_swap(out, first1);
				++out;
				++first1;
			}
		}

		return out;
	}

	// WARNING: this algorithm is only ok for sorting up to N items! end-start <= N
	template<typename It, typename Compare = std::less<>, size_t N = 256>
	always_force_inline constexpr void small_merge_sort_size(
					It start, It end, Compare comp = Compare{}, size_t diff = N)
	{
		using value_type = sort::iter_value_t<It>;
		std::array<value_type, N> buffer;

		size_t stop   = (diff >> 2) + ((diff & 0x3) > 0);
		size_t stride = 1;

		auto b0 = buffer.data();
		for (;;) {
			auto   it      = start;
			size_t stride2 = stride << 1;
			size_t stride3 = stride2 + stride;
			size_t step    = stride << 2;
			for (; (end - it) >= step;) { // 1 -> 2 -> 4,  4 -> 8 -> 16, 16 -> 32 -> 64, 64 -> 128 -> 256
				auto m0 = it + stride;
				auto l0 = it + stride2;
				auto m1 = it + stride3;
				auto l1 = it + step;

				auto b1 = b0 + stride2;

				sort::merge(it, m0, m0, l0, b0, comp);
				sort::merge(l0, m1, m1, l1, b1, comp);
				sort::merge(b0, b1, b1, b0 + step, it, comp);

				it += step;
			}

			if (it < end) { //
				size_t remaining = (end - it);
				auto   m0        = remaining >= stride ? it + stride : end;
				auto   l0        = remaining >= stride2 ? it + stride2 : end;
				auto   m1        = remaining >= stride3 ? it + stride3 : end;
				auto   l1        = remaining >= step ? it + step : end;

				auto lhs_out = sort::merge(it, m0, m0, l0, b0, comp);
				auto rhs_out = sort::merge(l0, m1, m1, l1, lhs_out, comp);
				sort::merge(b0, lhs_out, lhs_out, rhs_out, it, comp);
			}

			if (stride >= stop)
				break;

			stride = stride2;
		}
	}

	template<typename It, typename Compare = std::less<>>
	always_force_inline constexpr void small_merge_sort(It start, It end, Compare comp = Compare{})
	{
		return sort::small_merge_sort_size(start, end, comp, end - start);
	}

	template<class ForwardIt, class UnaryPred>
	always_force_inline constexpr ForwardIt partition(ForwardIt first, ForwardIt last, UnaryPred p)
	{
		for (;;) {
			if (first == last)
				return first;
			if (!p(*first))
				break;
			++first;
		}

		for (auto i = first; ++i != last;) {
			if (p(*i)) {
				sort::iter_swap(i, first);
				++first;
			}
		}

		return first;
	}

	template<class ForwardIt, class UnaryPred>
	always_force_inline constexpr ForwardIt partition_branchless(ForwardIt first, ForwardIt last, UnaryPred p)
	{
		for (;;) {
			if (first == last)
				return first;
			if (!p(*first))
				break;
			++first;
		}

		for (auto i = first; ++i != last;) {
			size_t r = p(*i);
			sort::iter_swap(i, first);
			first += r;
			// if (p(*i)) {
			//	sort::iter_swap(i, first);
			//	++first;
			// }
		}

		return first;
	}

	template<class ForwardIt, class UnaryPred>
	constexpr ForwardIt reversed_partition(ForwardIt first, const ForwardIt last, UnaryPred p)
	{
		for (;;) {
			if (first == last)
				return first;
			if (p(*first))
				break;
			++first;
		}

		for (auto i = first; ++i != last;) {
			if (!p(*i)) {
				sort::iter_swap(i, first);
				++first;
			}
		}

		return first;
	}

	template<typename It> always_force_inline constexpr void reverse(It start, It end)
	{
		for (; start < end; ++start) {
			--end;
			sort::iter_swap(start, end);
			// sort::swap_branchless_unconditional(*start, *end);
		}
	}

	struct counting_sort_bytes {
		uint64_t idxs      = {};
		uint8_t  bytes     = {};
		uint8_t  byte_idx  = {};
		uint8_t  processed = {};
	};

	// NOTE: only exists because I *thought* I found a bug in MSVC AND CLANG codegen...
	template<typename T> always_force_inline constexpr auto minimum_unsigned_value()
	{
		using unsigned_type = typename ::std::make_unsigned<T>::type;
		return ~((~(unsigned_type{0})) >> 1u); // make sure to use LOGICAL shift vs ARTHEMETIC
	}

	template<typename T> always_force_inline constexpr auto treat_as_unsigned_rshifted(T v, uint32_t shift)
	{
#if defined(cplusplus_version_20)
		if constexpr (::std::is_floating_point<T>::value) {
			if constexpr (::std::is_same<T, float>::value) {
				uint32_t uv        = std::bit_cast<uint32_t>(v);
				uint32_t sf        = uv >> (sizeof(uint32_t) * 8 - 1);
				uint32_t flip_mask = 0x80000000 | (0xffffffff * sf);
				return (uv ^ flip_mask) >> shift;
			} else if constexpr (::std::is_same<T, double>::value) {
				uint64_t uv        = ::std::bit_cast<uint64_t>(v);
				uint64_t sf        = uv >> (sizeof(uint64_t) * 8 - 1);
				uint64_t flip_mask = 0x8000000000000000 | (0xffffffffffffffff * sf);
				return (uv ^ flip_mask) >> shift;
			} else {
				static_assert(false, "type needs to be convertible to an unsigned integer to be used as a key, "
									 "floating types float and double are supported");
			}
		} else
#endif
#if defined __has_include
#if __has_include(<bitset>)
						if constexpr (is_bitset<T>::value) {
			if constexpr (v.size() <= 8) {
				return ((uint8_t)v.to_ulong()) >> shift;
			} else if constexpr (v.size() <= 16) {
				return ((uint16_t)v.to_ulong()) >> shift;
			} else if constexpr (v.size() <= 32) {
				return v.to_ulong() >> shift;
			} else if constexpr (v.size() <= 64) {
				return v.to_ullong() >> shift;
			} else {
				return ((~T{0} >> (v.size() - 64)) & (v >> shift)).to_ullong();
			}
		} else
#endif
#endif
						if constexpr (::std::is_integral<T>::value && ::std::is_signed<T>::value) {
			using unsigned_type = typename ::std::make_unsigned<T>::type;
			// constexpr unsigned_type min_value = {~((~unsigned_type{0}) >> 1)};
			constexpr unsigned_type min_value = sort::minimum_unsigned_value<T>();
			return (unsigned_type)(((unsigned_type)v + min_value) >> shift);
		} else if constexpr (::std::is_integral<T>::value) {
			return v >> shift;
		} else {
			static_assert(false, "type needs to be convertible to an unsigned integer to be used as a key");
			return v;
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
			} else if constexpr (::std::is_same<T, double>::value) {
				uint64_t uv        = ::std::bit_cast<uint64_t>(v);
				uint64_t sf        = uv >> (sizeof(uint64_t) * 8 - 1);
				uint64_t flip_mask = 0x8000000000000000 | (0xffffffffffffffff * sf);
				return uv ^ flip_mask;
			} else {
				static_assert(false, "type needs to be convertible to an unsigned integer to be used as a key, "
									 "floating types float and double are supported");
			}
		} else
#endif
#if defined __has_include
#if __has_include(<bitset>)
						if constexpr (is_bitset<T>::value) {
			if constexpr (v.size() <= 8) {
				return (uint8_t)v.to_ulong();
			} else if constexpr (v.size() <= 16) {
				return (uint16_t)v.to_ulong();
			} else if constexpr (v.size() <= 32) {
				return v.to_ulong();
			} else if constexpr (v.size() <= 64) {
				return v.to_ullong();
			} else {
				return (v >> (64 - v.size())).to_ullong();
				// static_assert(false, "bitsets up to size 64 are supported");
			}
		} else
#endif
#endif
						if constexpr (::std::is_integral<T>::value && ::std::is_signed<T>::value) {
			using unsigned_type               = typename ::std::make_unsigned<T>::type;
			constexpr unsigned_type min_value = sort::minimum_unsigned_value<T>();
			return (unsigned_type)v + min_value;
		} else if constexpr (::std::is_integral<T>::value) {
			return v;
		} else {
			static_assert(false, "type needs to be convertible to an unsigned integer to be used as a key");
			return v;
		}
	}

	template<typename T> constexpr size_t bytes_required(T v)
	{
#if defined __has_include
#if __has_include(<bitset>)
		if constexpr (is_bitset<T>::value) {
			return (v.size() / 8) + (v.size() % 8) > 0;
		} else
#else
#endif
#endif
						if constexpr (true) {
			return sizeof(T);
		}
	}

	template<typename T> constexpr size_t bits_required(T v)
	{
#if defined __has_include
#if __has_include(<bitset>)
		if constexpr (is_bitset<T>::value) {
			return v.size();
		} else
#else
#endif
#endif
						if constexpr (true) {
			return sizeof(T) * 8;
		}
	}

	template<typename T> constexpr size_t bitset_size(T)
	{
		return sizeof(T) * 8;
	}

	template<size_t N> constexpr size_t bitset_size(std::bitset<N>)
	{
		return N;
	}

	template<typename Callback, size_t... Idxs> struct defer_callback {
		using callback = Callback;
		using idxs     = std::index_sequence<Idxs...>;
	};

	template<typename Callback, size_t... Idxs> struct defer_callback_reversed {
		using callback = Callback;
		using idxs     = std::index_sequence<Idxs...>;
	};

	template<size_t... Idxs>
	constexpr auto make_reversed_index_sequence(std::index_sequence<Idxs...>)
					-> std::index_sequence<((sizeof...(Idxs) - 1) - Idxs)...>
	{
		return {};
	}

	template<typename It, typename ExtractKey, size_t N = 0>
	always_force_inline constexpr auto retrieve_key(It it, ExtractKey extract_key)
	{
		using extract_type = decltype(ExtractKey{}(*std::declval<It>()));
		if constexpr (sort::is_tuple<extract_type>::value) {
			return std::get<N>(extract_key(*it));
		} else if constexpr (sort::is_array<extract_type>::value) {
			return std::get<N>(extract_key(*it));
		} else {
			return extract_key(*it);
		}
	}

	template<typename T, typename ExtractKey, size_t N = 0>
	always_force_inline constexpr auto retrieve_key_value(const T& value, ExtractKey extract_key)
	{
		using extract_type = decltype(ExtractKey{}(std::declval<T>()));
		if constexpr (sort::is_tuple<extract_type>::value) {
			return std::get<N>(extract_key(value));
		} else if constexpr (sort::is_array<extract_type>::value) {
			return std::get<N>(extract_key(value));
		} else {
			return extract_key(value);
		}
	}

	template<typename index_type> struct counting_sort_memory {
		static constexpr size_t index_size     = sizeof(index_type);
		static constexpr size_t cacheline_size = std::hardware_constructive_interference_size;

		// The start of one index is the end of another, we can compress the data into
		// index pairs right next to each other
		alignas(16) index_type counts[256];
		// make sure counts[x] and stack_data[x] are not on the same cacheline
		alignas(16) uint8_t idxs[256];
		alignas(16) index_type stack_data[257];
	};

	template<typename It, typename ExtractKey, size_t Idx, size_t... Idxs, typename... Deferred>
	constexpr void counting_sort_recursive(It start, It end, ExtractKey extract_key,
					std::index_sequence<Idx, Idxs...> = {}, sort::parameter_list<Deferred...> = {},
					sort::counting_sort_bytes remaining = {})
	{
		using namespace sort;
		using value_type   = sort::iter_value_t<It>;
		using extract_type = decltype(ExtractKey{}(*std::declval<It>()));
		using key_type     = sort::remove_cvref_t<decltype(sort::retrieve_key<It, ExtractKey, Idx>(
                        std::declval<It>(), ExtractKey{}))>;
		// sort::remove_cvref_t<decltype(::std::get<Idx>(ExtractKey{}(*std::declval<It>())))>; // this works for both
		// tuples and arrays
		if constexpr (sort::is_tuple<key_type>::value) {
			remaining.bytes    = 0;
			remaining.byte_idx = 0;
			if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
							::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
				if constexpr (sizeof...(Idxs)) {
					return sort::counting_sort_recursive(start, end,
									sort::wrapped_greater_than{[](const auto& v) { return ::std::get<Idx>(v); }},
									std::make_index_sequence<std::tuple_size<key_type>::value>{},
									parameter_list<defer_callback<ExtractKey, Idxs...>, Deferred...>{}, remaining);
				} else { // if we're on the last item of a tuple, don't push a deferred callback onto the template stack
					return sort::counting_sort_recursive(start, end,
									sort::wrapped_greater_than{[](const auto& v) { return ::std::get<Idx>(v); }},
									std::make_index_sequence<std::tuple_size<key_type>::value>{},
									parameter_list<Deferred...>{}, remaining);
				}
			} else if constexpr (sort::is_wrapped_greater_than<ExtractKey>::value) {
				if constexpr (sizeof...(Idxs)) {
					return sort::counting_sort_recursive(start, end, sort::wrapped_greater_than{[](const auto& v) {
						return ::std::get<Idx>(ExtractKey{}(v));
					}},
									std::make_index_sequence<std::tuple_size<key_type>::value>{},
									parameter_list<defer_callback<ExtractKey, Idxs...>, Deferred...>{}, remaining);
				} else { // if we're on the last item of a tuple, don't push a deferred callback onto the template stack
					return sort::counting_sort_recursive(start, end, sort::wrapped_greater_than{[](const auto& v) {
						return ::std::get<Idx>(ExtractKey{}(v));
					}},
									std::make_index_sequence<std::tuple_size<key_type>::value>{},
									parameter_list<Deferred...>{}, remaining);
				}
			} else {
				if constexpr (sizeof...(Idxs)) {
					return sort::counting_sort_recursive(
									start, end, [](const auto& v) { return ::std::get<Idx>(ExtractKey{}(v)); },
									std::make_index_sequence<std::tuple_size<key_type>::value>{},
									parameter_list<defer_callback<ExtractKey, Idxs...>, Deferred...>{}, remaining);
				} else { // if we're on the last item of a tuple, don't push a deferred callback onto the template stack
					return sort::counting_sort_recursive(
									start, end, [](const auto& v) { return ::std::get<Idx>(ExtractKey{}(v)); },
									std::make_index_sequence<std::tuple_size<key_type>::value>{},
									parameter_list<Deferred...>{}, remaining);
				}
			}
		} else if constexpr (std::is_same<key_type, bool>::value
#if defined __has_include
#if __has_include(<bitset>)
							 || (is_bitset<key_type>::value && bitset_size(key_type()) == 1)
#endif
#endif
		) {
#if defined __has_include
#if __has_include(<bitset>)
			using max_key_type = typename std::conditional<is_bitset<key_type>::value, key_type,
							decltype(sort::treat_as_unsigned_rshifted(std::declval<key_type>(), 0))>::type;
#else
			using max_key_type = decltype(sort::treat_as_unsigned_rshifted(std::declval<key_type>(), 0));
#endif
#else
			using max_key_type = decltype(sort::treat_as_unsigned_rshifted(std::declval<key_type>(), 0));
#endif

			It split;
			if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
							::std::is_same<identity_greater_than<key_type>, ExtractKey>::value ||
							sort::is_wrapped_greater_than<ExtractKey>::value) {
				if constexpr (std::is_same<key_type, bool>::value) {
					split = sort::partition_branchless(start, end, [](const auto& v) { return ExtractKey{}(v); });
				} else {
					split = sort::partition_branchless(start, end, [](const auto& v) { return ExtractKey{}(v)[0]; });
				}
			} else {
				if constexpr (std::is_same<key_type, bool>::value) {
					split = sort::partition_branchless(start, end, [](const auto& v) { return !ExtractKey{}(v); });
				} else {
					split = sort::partition_branchless(start, end, [](const auto& v) { return !ExtractKey{}(v)[0]; });
				}
			}
			if constexpr ((sizeof...(Idxs)) || (sizeof...(Deferred))) {
				remaining.bytes    = 0;
				remaining.byte_idx = 0;
				if constexpr (sizeof...(Idxs)) {
					sort::counting_sort_recursive(start, split, extract_key, std::index_sequence<Idxs...>{},
									parameter_list<Deferred...>{});
					return sort::counting_sort_recursive(split, end, extract_key, std::index_sequence<Idxs...>{},
									parameter_list<Deferred...>{}, remaining);
				} else {
					using popped_list    = decltype(sort::pop_front(parameter_list<Deferred...>{}));
					using first_deferred = decltype(sort::front(parameter_list<Deferred...>{}));
					sort::counting_sort_recursive(start, split, typename first_deferred::callback{},
									typename first_deferred::idxs{}, popped_list{}, remaining);
					return sort::counting_sort_recursive(split, end, typename first_deferred::callback{},
									typename first_deferred::idxs{}, popped_list{}, remaining);
				}
			}
		} else if constexpr (sort::is_array<key_type>::value) {
			remaining.bytes    = 0;
			remaining.byte_idx = 0;
			if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
							::std::is_same<identity_greater_than<key_type>, ExtractKey>::value ||
							sort::is_wrapped_greater_than<ExtractKey>::value) {
				if constexpr (sizeof...(Idxs)) {
					return sort::counting_sort_recursive(start, end, sort::wrapped_greater_than{[](const auto& v) {
						return ::std::get<Idx>(ExtractKey{}(v));
					}},
									sort::make_reversed_index_sequence(
													std::make_index_sequence<std::tuple_size<key_type>::value>{}),
									parameter_list<defer_callback<ExtractKey, Idxs...>, Deferred...>{}, remaining);
				} else { // if we're on the last item of a tuple, don't push a deferred callback onto the template stack
					return sort::counting_sort_recursive(start, end, sort::wrapped_greater_than{[](const auto& v) {
						return ::std::get<Idx>(ExtractKey{}(v));
					}},
									sort::make_reversed_index_sequence(
													std::make_index_sequence<std::tuple_size<key_type>::value>{}),
									parameter_list<Deferred...>{}, remaining);
				}
			} else {
				if constexpr (sizeof...(Idxs)) {
					return sort::counting_sort_recursive(
									start, end, [](const auto& v) { return ::std::get<Idx>(ExtractKey{}(v)); },
									sort::make_reversed_index_sequence(
													std::make_index_sequence<std::tuple_size<key_type>::value>{}),
									parameter_list<defer_callback<ExtractKey, Idxs...>, Deferred...>{}, remaining);
				} else { // if we're on the last item of a tuple, don't push a deferred callback onto the template stack
					return sort::counting_sort_recursive(
									start, end, [](const auto& v) { return ::std::get<Idx>(ExtractKey{}(v)); },
									sort::make_reversed_index_sequence(
													std::make_index_sequence<std::tuple_size<key_type>::value>{}),
									parameter_list<Deferred...>{}, remaining);
				}
			}
		} else {
#if defined __has_include
#if __has_include(<bitset>)
			using max_key_type = typename std::conditional<is_bitset<key_type>::value, key_type,
							decltype(sort::treat_as_unsigned_rshifted(std::declval<key_type>(), 0))>::type;
#else
			using max_key_type = decltype(sort::treat_as_unsigned_rshifted(std::declval<key_type>(), 0));
#endif
#else
			using max_key_type = decltype(sort::treat_as_unsigned_rshifted(std::declval<key_type>(), 0));
#endif
			using index_type = size_t;
			static_assert(std::is_integral<key_type>::value
#if defined __has_include
#if __has_include(<bitset>)
											|| is_bitset<key_type>::value
#endif
#endif
#if defined(cplusplus_version_20)
											|| std::is_same<key_type, float>::value ||
											std::is_same<key_type, double>::value
#endif
							,
							"::std::get<Idx>(extract_key(*it)) must return a key type!");
			constexpr bool is_forward_iterator = std::is_same<typename std::iterator_traits<It>::iterator_category,
							std::forward_iterator_tag>::value;
			constexpr bool is_bidirectional_iterator =
							std::is_same<typename std::iterator_traits<It>::iterator_category,
											std::bidirectional_iterator_tag>::value;
			constexpr bool can_small_sort = ::std::is_default_constructible<value_type>::value &&
											std::is_same<typename ::std::iterator_traits<It>::iterator_category,
															::std::random_access_iterator_tag>::value;
			// constexpr size_t initial_count_indexs      = 256 * sizeof(key_type);
			constexpr size_t required_start_end_indexs = 257 * sizeof(key_type);
			constexpr size_t count_indexs              = 256;
			constexpr size_t start_end_indexs          = 257;
			constexpr size_t iterator_count            = is_forward_iterator || is_bidirectional_iterator ? 257 : 1;

			counting_sort_memory<index_type> stack;

			uint32_t bit_shift;
			uint8_t  is_ordered;
			uint8_t  last_key;
			uint16_t fallback0_count;
			uint16_t fallback1_count;
			uint16_t recursion_count;

			It iterators[iterator_count];

			{
				sort::counting_sort_bytes next{};
				for (uint64_t x = sizeof(key_type); --x < sizeof(key_type);) {
					next.idxs |= (x << (8 * next.bytes));
					next.bytes += 1;
				}
				remaining.idxs  = next.idxs;
				remaining.bytes = next.bytes;
			}
			uint32_t x      = remaining.bytes ? ((remaining.idxs >> (remaining.byte_idx << 3)) & 0xff)
											  : sizeof(key_type) - 1;
			bit_shift       = x << 3;
			fallback0_count = 0;
			fallback1_count = 0;
			recursion_count = 0;
			is_ordered      = 1;
			last_key        = 0;

			for (size_t i = 0; i < 257; i++) {
				stack.stack_data[i] = 0;
				// counts[i] = 0;
			}

			size_t total_items = 0;
			for (It it = start; it != end; ++it) {
				key_type k;
				uint8_t  key_byte;
				if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
								::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
					if constexpr (sort::is_tuple<extract_type>::value) {
						k = std::get<Idx>(*it);
					} else if constexpr (sort::is_array<extract_type>::value) {
						k = std::get<Idx>(*it);
					} else {
						k = *it;
					}
				} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
									 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
					if constexpr (sort::is_tuple<extract_type>::value) {
						k = std::get<Idx>(*it);
					} else if constexpr (sort::is_array<extract_type>::value) {
						k = std::get<Idx>(*it);
					} else {
						k = *it;
					}
				} else {
					if constexpr (sort::is_tuple<extract_type>::value) {
						k = std::get<Idx>(extract_key(*it));
					} else if constexpr (sort::is_array<extract_type>::value) {
						k = std::get<Idx>(extract_key(*it));
					} else {
						k = extract_key(*it);
					}
				}

				if constexpr (::std::is_integral<key_type>::value) {
					if constexpr (::std::is_signed<key_type>::value) {
						using unsigned_type               = typename ::std::make_unsigned<max_key_type>::type;
						constexpr unsigned_type min_value = sort::minimum_unsigned_value<max_key_type>();

						unsigned_type uk = k + min_value;
						key_byte         = uk >> bit_shift;
					} else {
						key_byte = k >> bit_shift;
					}
				} else {
					key_byte = sort::treat_as_unsigned_rshifted(k, bit_shift);
				}
				// reverse the sort direction by inverting the key
				if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
								::std::is_same<identity_greater_than<key_type>, ExtractKey>::value ||
								sort::is_wrapped_greater_than<ExtractKey>::value) {
					key_byte = ~key_byte;
				}
				// counts[key_byte] = total_items;
				is_ordered &= (uint8_t)(key_byte >= last_key);
				last_key = key_byte;

				++total_items;
				++stack.stack_data[key_byte];
			}

			It start_it;
			{ // convert counts to prefix sum
				size_t   idx          = 0;
				size_t   total        = 0;
				size_t   prev_idx     = 0;
				uint16_t current_byte = (remaining.idxs >> (remaining.byte_idx << 3)) & 0xff;

				// NOTE: optimize this furthur
				if constexpr (is_forward_iterator || is_bidirectional_iterator) {
					start_it = start;
				}

				for (; idx < 256;) {
					size_t count[4];

					size_t tidx  = idx;
					size_t tidx1 = idx + 1;
					size_t tidx2 = idx + 2;
					size_t tidx3 = idx + 3;
					idx += 4;

					count[0] = stack.stack_data[tidx];
					count[1] = stack.stack_data[tidx1];
					count[2] = stack.stack_data[tidx2];
					count[3] = stack.stack_data[tidx3];

					if constexpr (is_forward_iterator || is_bidirectional_iterator) {
						iterators[tidx] = start_it;
						std::advance(start_it, count[0]);
						iterators[tidx1] = start_it;
						std::advance(start_it, count[1]);
						iterators[tidx2] = start_it;
						std::advance(start_it, count[2]);
						iterators[tidx3] = start_it;
						std::advance(start_it, count[3]);
					}

					stack.idxs[recursion_count]       = tidx;
					stack.idxs[255 - fallback1_count] = tidx;
					if constexpr (can_small_sort) {
						recursion_count += count[0] > small_merge_sort_threshold;
						fallback1_count += count[0] > 1 && count[0] <= small_merge_sort_threshold;
					} else if constexpr (is_forward_iterator || is_bidirectional_iterator) {
						recursion_count += count[0] > 2;
						fallback1_count += count[0] == 2;
					} else {
						recursion_count += count[0] > intro_sort_threshold;
						fallback1_count += count[0] > insertion_sort_threshold && count[0] <= intro_sort_threshold;
					}

					stack.counts[tidx]     = total;
					stack.stack_data[tidx] = total;
					total += count[0];

					stack.idxs[recursion_count]       = tidx1;
					stack.idxs[255 - fallback1_count] = tidx1;
					if constexpr (can_small_sort) {
						recursion_count += count[1] > small_merge_sort_threshold;
						fallback1_count += count[1] > 1 && count[1] <= small_merge_sort_threshold;
					} else if constexpr (is_forward_iterator || is_bidirectional_iterator) {
						recursion_count += count[1] > 2;
						fallback1_count += count[1] == 2;
					} else {
						recursion_count += count[1] > intro_sort_threshold;
						fallback1_count += count[1] > insertion_sort_threshold && count[1] <= intro_sort_threshold;
					}

					stack.counts[tidx1]     = total;
					stack.stack_data[tidx1] = total;
					total += count[1];

					stack.idxs[recursion_count]       = tidx2;
					stack.idxs[255 - fallback1_count] = tidx2;
					if constexpr (can_small_sort) {
						recursion_count += count[2] > small_merge_sort_threshold;
						fallback1_count += count[2] > 1 && count[2] <= small_merge_sort_threshold;
					} else if constexpr (is_forward_iterator || is_bidirectional_iterator) {
						recursion_count += count[2] > 2;
						fallback1_count += count[2] == 2;
					} else {
						recursion_count += count[2] > intro_sort_threshold;
						fallback1_count += count[2] > insertion_sort_threshold && count[2] <= intro_sort_threshold;
					}

					stack.counts[tidx2]     = total;
					stack.stack_data[tidx2] = total;
					total += count[2];

					stack.idxs[recursion_count]       = tidx3;
					stack.idxs[255 - fallback1_count] = tidx3;
					if constexpr (can_small_sort) {
						recursion_count += count[3] > small_merge_sort_threshold;
						fallback1_count += count[3] > 1 && count[3] <= small_merge_sort_threshold;
					} else if constexpr (is_forward_iterator || is_bidirectional_iterator) {
						recursion_count += count[3] > 2;
						fallback1_count += count[3] == 2;
					} else {
						recursion_count += count[3] > intro_sort_threshold;
						fallback1_count += count[3] > insertion_sort_threshold && count[3] <= intro_sort_threshold;
					}

					stack.counts[tidx3]     = total;
					stack.stack_data[tidx3] = total;
					total += count[3];
				}

				stack.stack_data[256] = total;
				if constexpr (is_forward_iterator || is_bidirectional_iterator) {
					iterators[256] = start_it; // end;
				}

				if constexpr (!can_small_sort) {
					size_t remaining = 256 - (fallback1_count + recursion_count);
					for (idx = 0; idx < 256 && remaining; idx++) {
						size_t count = stack.stack_data[idx + 1] - stack.stack_data[idx];
						stack.idxs[255 - (fallback1_count + fallback0_count)] = idx;
						uint8_t within_fallback0_range = count > 1 && count <= insertion_sort_threshold;
						fallback0_count += within_fallback0_range;
						remaining -= within_fallback0_range;
					}
				}
			}

			start_it = start;

			if (!is_ordered) {
				size_t sorted_count = 0;
				do {
					if constexpr (is_forward_iterator || is_bidirectional_iterator) {
						for (size_t x = 0; x < 256; x++) {
							size_t s = stack.counts[x];         // counts[depth][x];
							size_t e = stack.stack_data[x + 1]; // start_end[depth][x + 1];
							sorted_count += (e - s);
							It start_left = iterators[x];
							for (; s < e; s++) {
								It swap_left = start_left;
								++start_left;
								It swap_target;

								key_type k;
								uint8_t  key_byte;
								if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
												::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
									if constexpr (sort::is_tuple<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else if constexpr (sort::is_array<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else {
										k = *swap_left;
									}
								} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
													 ::std::is_same<identity_greater_than<key_type>,
																	 ExtractKey>::value) {
									if constexpr (sort::is_tuple<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else if constexpr (sort::is_array<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else {
										k = *swap_left;
									}
								} else {
									if constexpr (sort::is_tuple<extract_type>::value) {
										k = std::get<Idx>(extract_key(*swap_left));
									} else if constexpr (sort::is_array<extract_type>::value) {
										k = std::get<Idx>(extract_key(*swap_left));
									} else {
										k = extract_key(*swap_left);
									}
								}

								if constexpr (::std::is_integral<key_type>::value) {
									if constexpr (::std::is_signed<key_type>::value) {
										using unsigned_type = typename ::std::make_unsigned<max_key_type>::type;
										constexpr unsigned_type min_value =
														sort::minimum_unsigned_value<max_key_type>();

										unsigned_type uk = k + min_value;
										key_byte         = uk >> bit_shift;
									} else {
										key_byte = k >> bit_shift;
									}
								} else {
									key_byte = sort::treat_as_unsigned_rshifted(k, bit_shift);
								}

								// reverse the sort direction by inverting the key
								if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
												::std::is_same<identity_greater_than<key_type>, ExtractKey>::value ||
												sort::is_wrapped_greater_than<ExtractKey>::value) {
									key_byte = ~key_byte;
								}

								// size_t target_idx = stack.counts[key_byte];
								swap_target = iterators[key_byte]; // start_it + target_idx;

								// sort::swap_branchless_unconditional(*swap_left, *swap_target);
								sort::iter_swap(swap_left, swap_target);
								stack.counts[key_byte] += 1;
								++iterators[key_byte];
							}
						}
					} else {
						for (size_t x = 0; x < 256; x++) {
							size_t s = stack.counts[x];         // counts[depth][x];
							size_t e = stack.stack_data[x + 1]; // start_end[depth][x + 1];
							// this is so when we loop back around we start past the point we
							// know the data is sorted, skarupke mention's swapping things around
							// I'm not convinced that's a good idea, plus this is easy to program anyway
							sorted_count += (e - s);
							for (; s < e; s++) {
								It swap_left = start_it + s;
								It swap_target;

								key_type k;
								uint8_t  key_byte;
								if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
												::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
									if constexpr (sort::is_tuple<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else if constexpr (sort::is_array<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else {
										k = *swap_left;
									}
								} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
													 ::std::is_same<identity_greater_than<key_type>,
																	 ExtractKey>::value) {
									if constexpr (sort::is_tuple<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else if constexpr (sort::is_array<extract_type>::value) {
										k = std::get<Idx>(*swap_left);
									} else {
										k = *swap_left;
									}
								} else {
									if constexpr (sort::is_tuple<extract_type>::value) {
										k = std::get<Idx>(extract_key(*swap_left));
									} else if constexpr (sort::is_array<extract_type>::value) {
										k = std::get<Idx>(extract_key(*swap_left));
									} else {
										k = extract_key(*swap_left);
									}
								}

								if constexpr (::std::is_integral<key_type>::value) {
									if constexpr (::std::is_signed<key_type>::value) {
										using unsigned_type = typename ::std::make_unsigned<max_key_type>::type;
										constexpr unsigned_type min_value =
														sort::minimum_unsigned_value<max_key_type>();

										unsigned_type uk = k + min_value;
										key_byte         = uk >> bit_shift;
									} else {
										key_byte = k >> bit_shift;
									}
								} else {
									key_byte = sort::treat_as_unsigned_rshifted(k, bit_shift);
								}

								// reverse the sort direction by inverting the key
								if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
												::std::is_same<identity_greater_than<key_type>, ExtractKey>::value ||
												sort::is_wrapped_greater_than<ExtractKey>::value) {
									key_byte = ~key_byte;
								}

								size_t target_idx = stack.counts[key_byte];
								swap_target       = start_it + target_idx;

								sort::swap_branchless_unconditional(*swap_left, *swap_target);
								stack.counts[key_byte] += 1;
							}
						}
					}
				} while (sorted_count < stack.stack_data[256]);

				if constexpr (is_forward_iterator || is_bidirectional_iterator) {
					// NOTE: only applicable to forward/bidirectional iterators*
					// We're shifting the iterators to the right because if we needed
					// to swap things into place these iterators would be incremented
					// (effectively shifting them to the left).

					// The recursion assumes they're in the same starting state as the top of the loop
					// that's what we're correcting for.
					for (size_t i = 256; --i > 0;) {
						iterators[i] = iterators[i - 1];
					}
					iterators[0] = start;
				}
			}
			// no recursion needed, every item had a dedicated location
			// if (partitions == total_items) // the branchless approach doesn't need this, handled by the fact we skip
			// 	return;
			if constexpr ((sizeof...(Idxs)) == 0 && (sizeof...(Deferred) == 0)) {
				if (remaining.bytes <= 1) // we have other parts of the key to extract
					return;
			}

			if constexpr (is_forward_iterator || is_bidirectional_iterator) {
				for (uint16_t p = 0; p < fallback1_count; p++) {
					uint8_t next_i    = stack.idxs[255 - p];
					It      second_it = iterators[next_i];
					It      first_it  = second_it;
					++second_it;

					key_type k;
					key_type k1;

					if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
									::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
						if constexpr (sort::is_tuple<extract_type>::value) {
							k  = std::get<Idx>(*first_it);
							k1 = std::get<Idx>(*second_it);
						} else if constexpr (sort::is_array<extract_type>::value) {
							k  = std::get<Idx>(*first_it);
							k1 = std::get<Idx>(*second_it);
						} else {
							k  = *first_it;
							k1 = *second_it;
						}
					} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
										 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
						if constexpr (sort::is_tuple<extract_type>::value) {
							k  = std::get<Idx>(*first_it);
							k1 = std::get<Idx>(*second_it);
						} else if constexpr (sort::is_array<extract_type>::value) {
							k  = std::get<Idx>(*first_it);
							k1 = std::get<Idx>(*second_it);
						} else {
							k  = *first_it;
							k1 = *second_it;
						}
					} else {
						if constexpr (sort::is_tuple<extract_type>::value) {
							k  = std::get<Idx>(extract_key(*first_it));
							k1 = std::get<Idx>(extract_key(*second_it));
						} else if constexpr (sort::is_array<extract_type>::value) {
							k  = std::get<Idx>(extract_key(*first_it));
							k1 = std::get<Idx>(extract_key(*second_it));
						} else {
							k  = extract_key(*first_it);
							k1 = extract_key(*second_it);
						}
					}

					if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
									::std::is_same<identity_greater_than<key_type>, ExtractKey>::value ||
									sort::is_wrapped_greater_than<ExtractKey>::value) {
						sort::iter_swap_conditional(first_it, second_it, k1 > k); // k > k1
					} else {
						sort::iter_swap_conditional(first_it, second_it, k < k1); // k1 > k
					}
				}

				if constexpr (sizeof...(Idxs) || sizeof...(Deferred)) {
					if ((remaining.byte_idx + 1) >= remaining.bytes) {
						remaining.bytes    = 0;
						remaining.byte_idx = 0;
						for (uint16_t p = 0; p < recursion_count; p++) {
							uint8_t next_i = stack.idxs[p];

							// go one key deeper
							if constexpr (sizeof...(Idxs)) {
								sort::counting_sort_recursive(iterators[next_i], iterators[next_i + 1], extract_key,
												std::index_sequence<Idxs...>{}, parameter_list<Deferred...>{},
												remaining);
							} else {
								using popped_list    = decltype(sort::pop_front(parameter_list<Deferred...>{}));
								using first_deferred = decltype(sort::front(parameter_list<Deferred...>{}));
								sort::counting_sort_recursive(iterators[next_i], iterators[next_i + 1],
												typename first_deferred::callback{}, typename first_deferred::idxs{},
												popped_list{}, remaining);
							}
						}
					} else {
						remaining.byte_idx += 1;
						for (uint16_t p = 0; p < recursion_count; p++) {
							uint8_t next_i = stack.idxs[p];

							sort::counting_sort_recursive(iterators[next_i], iterators[next_i + 1], extract_key,
											std::index_sequence<Idx, Idxs...>{}, parameter_list<Deferred...>{},
											remaining);
						}
					}
				} else {
					if (remaining.byte_idx >= remaining.bytes) {
					} else {
						remaining.byte_idx += 1;
						for (uint16_t p = 0; p < recursion_count; p++) {
							uint8_t next_i = stack.idxs[p];

							sort::counting_sort_recursive(iterators[next_i], iterators[next_i + 1], extract_key,
											std::index_sequence<Idx, Idxs...>{}, parameter_list<Deferred...>{},
											remaining);
						}
					}
				}

			} else {
				if constexpr (can_small_sort) {
					for (uint16_t p = 0; p < fallback1_count; p++) {
						uint8_t i            = stack.idxs[255 - p];
						size_t  start_offset = stack.stack_data[i];
						size_t  end_offset   = stack.stack_data[i + 1];
						sort::small_merge_sort_size(
										start_it + start_offset, start_it + end_offset,
										[](const auto& lhs, const auto& rhs) {
											return ExtractKey{}(lhs) < ExtractKey{}(rhs);
										},
										end_offset - start_offset);
					}
				} else {
					for (uint16_t p = 0; p < fallback0_count; p++) {
						uint8_t i            = stack.idxs[255 - (p + fallback1_count)];
						size_t  start_offset = stack.stack_data[i];
						size_t  end_offset   = stack.stack_data[i + 1];
						sort::insertion_sort(start_it + start_offset, start_it + end_offset,
										[](const auto& lhs, const auto& rhs) {
											return ExtractKey{}(lhs) < ExtractKey{}(rhs);
										});
					}

					for (uint16_t p = 0; p < fallback1_count; p++) {
						uint8_t i            = stack.idxs[255 - p];
						size_t  start_offset = stack.stack_data[i];
						size_t  end_offset   = stack.stack_data[i + 1];
						sort::make_heap(start_it + start_offset, start_it + end_offset,
										[](const auto& lhs, const auto& rhs) {
											return ExtractKey{}(lhs) < ExtractKey{}(rhs);
										});
						sort::sort_heap(start_it + start_offset, start_it + end_offset,
										[](const auto& lhs, const auto& rhs) {
											return ExtractKey{}(lhs) < ExtractKey{}(rhs);
										});
					}
				}

				if (remaining.processed < 8) {
					if constexpr (sizeof...(Idxs) || sizeof...(Deferred)) {
						if ((remaining.byte_idx + 1) >= remaining.bytes) {
							remaining.bytes    = 0;
							remaining.byte_idx = 0;
							for (uint16_t p = 0; p < recursion_count; p++) {
								uint8_t next_i = stack.idxs[p];

								size_t start_offset = stack.stack_data[next_i];
								size_t end_offset   = stack.stack_data[next_i + 1];

								// go one key deeper
								if constexpr (sizeof...(Idxs)) {
									sort::counting_sort_recursive(start_it + start_offset, start_it + end_offset,
													extract_key, std::index_sequence<Idxs...>{},
													parameter_list<Deferred...>{}, remaining);
								} else {
									using popped_list    = decltype(sort::pop_front(parameter_list<Deferred...>{}));
									using first_deferred = decltype(sort::front(parameter_list<Deferred...>{}));
									sort::counting_sort_recursive(start_it + start_offset, start_it + end_offset,
													typename first_deferred::callback{},
													typename first_deferred::idxs{}, popped_list{}, remaining);
								}
							}
						} else {
							remaining.byte_idx += 1;
							for (uint16_t p = 0; p < recursion_count; p++) {
								uint8_t next_i = stack.idxs[p];

								size_t start_offset = stack.stack_data[next_i];
								size_t end_offset   = stack.stack_data[next_i + 1];

								sort::counting_sort_recursive(start_it + start_offset, start_it + end_offset,
												extract_key, std::index_sequence<Idx, Idxs...>{},
												parameter_list<Deferred...>{}, remaining);
							}
						}
					} else {
						if (remaining.byte_idx >= remaining.bytes) {
						} else {
							remaining.byte_idx += 1;
							for (uint16_t p = 0; p < recursion_count; p++) {
								uint8_t next_i = stack.idxs[p];

								size_t start_offset = stack.stack_data[next_i];
								size_t end_offset   = stack.stack_data[next_i + 1];

								sort::counting_sort_recursive(start_it + start_offset, start_it + end_offset,
												extract_key, std::index_sequence<Idx, Idxs...>{},
												parameter_list<Deferred...>{}, remaining);
							}
						}
					}
				} else {
					// skarupe mentions a failover / panic mode when the # of bytes processed gets too large
					for (uint16_t p = 0; p < recursion_count; p++) {
						uint8_t i            = stack.idxs[p];
						size_t  start_offset = stack.stack_data[i];
						size_t  end_offset   = stack.stack_data[i + 1];
						sort::make_heap(start_it + start_offset, start_it + end_offset,
										[](const auto& lhs, const auto& rhs) {
											return ExtractKey{}(lhs) < ExtractKey{}(rhs);
										});
						sort::sort_heap(start_it + start_offset, start_it + end_offset,
										[](const auto& lhs, const auto& rhs) {
											return ExtractKey{}(lhs) < ExtractKey{}(rhs);
										});
					}
				}
			}
		}
	}

	template<typename> struct is_convertible_to_integrals : std::false_type {};

	template<typename T, typename... Ts>
	struct is_convertible_to_integrals<std::tuple<T, Ts...>>
		: std::bool_constant<(
						  (is_convertible_to_integrals<Ts>::value) && ... && is_convertible_to_integrals<T>::value)> {};

	template<typename T, size_t N>
	struct is_convertible_to_integrals<std::array<T, N>> : std::bool_constant<is_convertible_to_integrals<T>::value> {};

#if defined __has_include
#if __has_include(<bitset>)
	template<size_t N> struct is_convertible_to_integrals<std::bitset<N>> : std::true_type {};
#endif
#endif

#if defined __has_include
#if __has_include(<optional>)
	template<typename T>
	struct is_convertible_to_integrals<std::optional<T>> : std::bool_constant<is_convertible_to_integrals<T>::value> {};
#endif
#endif

	template<> struct is_convertible_to_integrals<bool> : std::true_type {};

	template<> struct is_convertible_to_integrals<uint64_t> : std::true_type {};
	template<> struct is_convertible_to_integrals<uint32_t> : std::true_type {};
	template<> struct is_convertible_to_integrals<uint16_t> : std::true_type {};
	template<> struct is_convertible_to_integrals<uint8_t> : std::true_type {};

	template<> struct is_convertible_to_integrals<int64_t> : std::true_type {};
	template<> struct is_convertible_to_integrals<int32_t> : std::true_type {};
	template<> struct is_convertible_to_integrals<int16_t> : std::true_type {};
	template<> struct is_convertible_to_integrals<int8_t> : std::true_type {};

#if defined(cplusplus_version_20)
	template<> struct is_convertible_to_integrals<float> : std::true_type {};
	template<> struct is_convertible_to_integrals<double> : std::true_type {};
#endif

	template<typename... key_types> constexpr size_t partition_count(std::tuple<key_types...>);

	template<typename key_type> constexpr size_t partition_count()
	{
		if constexpr (sort::is_tuple<key_type>::value) {
			return sort::partition_count(key_type());
			// leaf_partitions<decltype(::std::get<0>(std::declval<key_type>())), depth + 1>() && depth < 2;
		} else if constexpr (sort::is_array<key_type>::value) {
			size_t partitions_0 = sort::partition_count<decltype(::std::get<0>(std::declval<key_type>()))>();
			size_t partitions   = 1;
			for (size_t i = 0; i < ::std::tuple_size<key_type>::value; i++) {
				partitions = (partitions_0 * partitions) > partitions ? (partitions_0 * partitions) : partitions;
			}
			return partitions * (partitions_0 != 0);
#if defined __has_include
#if __has_include(<bitset>)
		} else if constexpr (sort::is_bitset<key_type>::value && sort::bitset_size(key_type{}) > 1) {
			return 0;
		} else if constexpr (sort::is_bitset<key_type>::value && sort::bitset_size(key_type{}) <= 1) {
			return 2;
#endif
#endif
#if defined __has_include
#if __has_include(<optional>)
		} else if constexpr (sort::is_optional<key_type>::value) {
			return 1 + sort::partition_count<typename key_type::value_type>();
#endif
#endif
		} else if constexpr (::std::is_same<key_type, bool>::value || ::std::is_same<key_type, const bool&>::value) {
			return 2;
		} else if constexpr (::std::is_integral<key_type>::value) {
			return 0;
		} else if constexpr (::std::is_floating_point<key_type>::value) {
			return 0;
		} else {
			return 0;
		}
	}

	template<typename... key_types> constexpr size_t partition_count(std::tuple<key_types...>)
	{
		size_t partitions = 1;
		((partitions = (sort::partition_count<key_types>() * partitions) > partitions
									   ? (sort::partition_count<key_types>() * partitions)
									   : partitions),
						...);
		bool any_zeroed = ((sort::partition_count<key_types>() == 0) || ... || false);
		return partitions * !any_zeroed;
	}

	template<typename key_type, size_t depth = 0> constexpr bool leaf_partitions()
	{
		if constexpr (is_tuple<key_type>::value) {
			return leaf_partitions<decltype(::std::get<0>(std::declval<key_type>())), depth + 1>() && depth < 2;
		} else if constexpr (is_array<key_type>::value) {
			return leaf_partitions<decltype(::std::get<0>(std::declval<key_type>())), depth + 1>() &&
				   ::std::tuple_size<key_type>::value < 2; // each array index
#if defined __has_include
#if __has_include(<bitset>)
		} else if constexpr (sort::is_bitset<key_type>::value && sort::bitset_size(key_type{}) > 1) {
			return false;
		} else if constexpr (sort::is_bitset<key_type>::value && sort::bitset_size(key_type{}) <= 1) {
			return true;
#endif
#endif
		} else if constexpr (::std::is_same<key_type, bool>::value || ::std::is_same<key_type, const bool&>::value) {
			return true;
		} else if constexpr (::std::is_integral<key_type>::value) {
			return false;
		} else if constexpr (::std::is_floating_point<key_type>::value) {
			return false;
		} else {
			return false;
		}
	}

	template<typename It, typename ExtractKey = sort::identity_less_than<>>
	constexpr void counting_sort(It start, It end, ExtractKey extract_key)
	{
		using key_type = sort::remove_cvref_t<decltype(ExtractKey{}(::std::move(*std::declval<It>())))>;
		constexpr size_t potential_partitions = sort::partition_count<key_type>();
		auto             f                    = sort::get_unwrapped(start);
		auto             l                    = sort::get_unwrapped(end);
		if constexpr ((potential_partitions == 0 || potential_partitions > 16) &&
						std::is_same<typename ::std::iterator_traits<It>::iterator_category,
										::std::random_access_iterator_tag>::value) {
			auto item_count = l - f;
			if constexpr (::std::is_default_constructible<key_type>::value &&
							std::is_same<typename ::std::iterator_traits<It>::iterator_category,
											::std::random_access_iterator_tag>::value) {
				if (item_count <= small_merge_sort_threshold) {
					if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
									::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
						return sort::small_merge_sort(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs < rhs;
						});
					} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
										 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
						return sort::small_merge_sort(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs > rhs;
						});
					} else {
						return sort::small_merge_sort(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
					}
				}
			} else {
				if (item_count <= insertion_sort_threshold) {
					if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
									::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
						return sort::insertion_sort(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs < rhs;
						});
					} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
										 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
						return sort::insertion_sort(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs > rhs;
						});
					} else {
						return sort::insertion_sort(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
					}
				}

				if (item_count <= intro_sort_threshold) {
					if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
									::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
						sort::make_heap(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs < rhs;
						});
						return sort::sort_heap(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs < rhs;
						});
					} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
										 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
						sort::make_heap(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs > rhs;
						});
						return sort::sort_heap(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return lhs > rhs;
						});
					} else {
						sort::make_heap(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
						return sort::sort_heap(f, l, [](const auto& lhs, const auto& rhs) {
							using namespace sort;
							return ExtractKey{}(lhs) < ExtractKey{}(rhs);
						});
					}
				}
			}
		}

		if constexpr (is_tuple<key_type>::value) {
			sort::counting_sort_recursive(f, l, extract_key,
							std::make_index_sequence<std::tuple_size<key_type>::value>{}, parameter_list<>{});
#if defined __has_include
#if __has_include(<bitset>)
		} else if constexpr (sort::is_bitset<key_type>::value && sort::bitset_size(key_type{}) > 1) {

			sort::counting_sort_recursive(f, l, extract_key, std::index_sequence<0>{}, parameter_list<>{});
		} else if constexpr (sort::is_bitset<key_type>::value && sort::bitset_size(key_type{}) <= 1) {
			if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
							::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
				sort::partition_branchless(f, l, [](const auto& value) { return (ExtractKey{}(value)[0]); });
			} else {
				sort::partition_branchless(f, l, [](const auto& value) { return !(ExtractKey{}(value)[0]); });
			}
#endif
#endif
		} else if constexpr (::std::is_same<key_type, bool>::value || ::std::is_same<key_type, const bool&>::value) {
			// partition puts things that return true first...but counting sort should treat this as a value so...we'll
			// flip the extract function to keep the semantics the same as expected
			sort::partition_branchless(f, l, [](const auto& value) { return !ExtractKey{}(value); });
#if defined __has_include
#if __has_include(<optional>)
		} else if constexpr (sort::is_optional<key_type>::value) {
			if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
							::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
				auto value_start_it = sort::partition_branchless(
								f, l, [](const auto& value) { return !ExtractKey{}(value).has_value(); });
				sort::counting_sort(value_start_it, end, sort::wrapped_greater_than([](const auto& value) {
					return ExtractKey{}(value).value();
				}));
			} else {
				auto last_value_it = sort::partition_branchless(
								f, l, [](const auto& value) { return ExtractKey{}(value).has_value(); });
				sort::counting_sort(
								start, last_value_it, [](const auto& value) { return ExtractKey{}(value).value(); });
			}
#endif
#endif
		} else if constexpr (sort::is_array<key_type>::value) {
			sort::counting_sort_recursive(f, l, extract_key,
							sort::make_reversed_index_sequence(
											std::make_index_sequence<std::tuple_size<key_type>::value>{}),
							parameter_list<>{});
		} else if constexpr (::std::is_integral<key_type>::value) {
			if constexpr (::std::is_same<identity_less_than<>, ExtractKey>::value ||
							::std::is_same<identity_less_than<key_type>, ExtractKey>::value) {
				if constexpr (::std::is_signed<key_type>::value) {
					using unsigned_type               = typename ::std::make_unsigned<key_type>::type;
					constexpr unsigned_type min_value = sort::minimum_unsigned_value<key_type>();
					sort::counting_sort_recursive(
									f, l,
									[](const auto& value) {
										return (typename ::std::make_unsigned<key_type>::type)value + min_value;
									},
									std::index_sequence<0>{}, parameter_list<>{});
				} else {
					sort::counting_sort_recursive(f, l, extract_key, std::index_sequence<0>{}, parameter_list<>{});
				}
			} else if constexpr (::std::is_same<identity_greater_than<>, ExtractKey>::value ||
								 ::std::is_same<identity_greater_than<key_type>, ExtractKey>::value) {
				if constexpr (::std::is_signed<key_type>::value) {
					using unsigned_type               = typename ::std::make_unsigned<key_type>::type;
					constexpr unsigned_type min_value = sort::minimum_unsigned_value<key_type>();
					sort::counting_sort_recursive(
									f, l,
									[](const key_type& value) {
										return (typename ::std::make_unsigned<key_type>::type) ~(value + min_value);
									},
									std::index_sequence<0>{}, parameter_list<>{});
				} else {
					sort::counting_sort_recursive(
									f, l, [](const key_type& value) { return ~value; }, std::index_sequence<0>{},
									parameter_list<>{});
				}
			} else {
				sort::counting_sort_recursive(f, l, extract_key, std::index_sequence<0>{}, parameter_list<>{});
			}
		} else {
			static_assert(false, "counting sort requires some form of integral or boolean convertible type!");
		}
	}

	template<class It, class Compare> constexpr void insertion_sort(It first, It last, Compare comp)
	{
		using T = sort::iter_value_t<It>;
		auto i  = first;
		if constexpr (is_std_less<Compare>::value) {
			return sort::insertion_sort(first, last, sort::less<>{});
		} else if constexpr (is_std_greater<Compare>::value) {
			return sort::insertion_sort(first, last, sort::greater<>{});
		} else {
			using namespace sort;
			if constexpr ((std::is_arithmetic<T>::value || std::is_same<T, bool>::value) &&
							std::is_same<sort::remove_cvref_t<decltype(*first)>,
											T>::value) { // this second line is to gaurd against proxy references /
														 // reference wrappers
				for (; i != last; ++i) { // performs about 25% better than below with simple types like below
					auto j = i;
					for (; j != first;) {
						auto& rhs = *j;
						auto& lhs = *(--j);
						if (!comp(rhs, lhs)) // lhs < rhs (should be lhs <= rhs, rhs >= lhs, !(rhs < lhs))
							break;
						sort::swap_branchless_unconditional(rhs, lhs);
					}
				}
			} else if constexpr (std::is_move_constructible<T>::value) {
				for (; i != last; ++i) {
					auto j = i;
					auto h = i;
					T    tmp(std::move(*j));

					for (; j != first;) {
						if (!comp(tmp, *(--j)))
							break;
						*h = std::move(*j);
						h  = j;
					}
					*h = std::move(tmp);
				}
			} else if constexpr (std::is_assignable<T, T>::value) {
				for (; i != last; ++i) {
					auto j = i;
					for (; j != first;) {
						auto r   = j;
						auto rhs = *j;
						auto lhs = *(--j);
						auto l   = j;
						if (!comp(rhs, lhs))
							break;
						sort::iter_swap(l, r);
					}
				}
			} else {
				static_assert(false, "type must be arthmetic, move constructible or assignable!");
			}
		}
	}

	template<class It, class Compare> constexpr void reverse_insertion_sort(It first, It last, Compare comp = Compare{})
	{
		using T = sort::iter_value_t<It>;
		auto i  = first;
		if constexpr ((std::is_arithmetic<T>::value || std::is_same<T, bool>::value) &&
						std::is_same<sort::remove_cvref_t<decltype(*first)>,
										T>::value) { // this second line is to gaurd against proxy references /
													 // reference wrappers
			for (; i != last; ++i) { // performs about 25% better than below with simple types like below
				auto j = i;
				for (; j != first;) {
					auto& rhs = *j;
					auto& lhs = *(--j);
					if (!comp(lhs, rhs)) // lhs < rhs (should be lhs <= rhs, rhs >= lhs, !(rhs < lhs))
						break;
					sort::swap_branchless_unconditional(lhs, rhs);
				}
			}
		} else if constexpr (std::is_move_constructible<T>::value) {
			for (; i != last; ++i) {
				auto j = i;
				auto h = i;
				T    tmp(std::move(*j));

				for (; j != first;) {
					if (!comp(*(--j), tmp))
						break;
					*h = std::move(*j);
					h  = j;
				}
				*h = std::move(tmp);
			}
		} else if constexpr (std::is_assignable<T, T>::value) {
			for (; i != last; ++i) {
				auto j = i;
				for (; j != first;) {
					auto r   = j;
					auto rhs = *j;
					auto lhs = *(--j);
					auto l   = j;
					if (!comp(lhs, rhs))
						break;
					sort::iter_swap(l, r);
				}
			}
		} else {
			static_assert(false, "type must be arthmetic, move constructible or assignable!");
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

		sort::push_heap_by_index(first, hole, top, std::forward<T>(val), comp);
	}

	template<typename It, typename Comp> constexpr void make_heap(It start, It end, Comp comp)
	{
		using diff  = typename sort::iter_difference_t<It>;
		diff bottom = end - start;
		for (diff hole = bottom >> 1; hole > 0;) {
			--hole;
			sort::iter_value_t<It> tmp(std::move(*(start + hole)));
			sort::pop_heap_hole_by_index(start, hole, bottom, ::std::move(tmp), comp);
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

	template<class It, class Comp = std::less<>> constexpr void pop_heap_unchecked(It start, It end, Comp comp)
	{
		// pop *start to *(end - 1) and reheap
		if (2 <= end - start) {
			--end;
			// decltype(*end)
			typename sort::iter_value_t<It> val(std::move(*end));
			sort::pop_heap_hole_unchecked(start, end, end, std::move(val), comp);
		}
	}

	template<typename It, typename Comp> constexpr void sort_heap(It start, It end, Comp comp)
	{
		for (; end - start >= 2; --end) {
			sort::pop_heap_unchecked(start, end, comp);
		}
	}

	template<class It, class Predicate>
	release_force_inline constexpr void med3_unchecked(It first, It mid, It last, Predicate pred)
	{
		// sort median of three elements to middle
		using T                            = sort::iter_value_t<It>;
		constexpr bool use_swap_branchless = (std::is_arithmetic<T>::value || std::is_same<T, bool>::value) &&
											 std::is_same<sort::remove_cvref_t<decltype(*first)>, T>::value;
		if (pred(*mid, *first)) {
			if constexpr (use_swap_branchless)
				sort::swap_branchless_unconditional(*mid, *first);
			else
				sort::iter_swap(mid, first);
		}

		if (pred(*last, *mid)) { // swap middle and last, then test first again
			if constexpr (use_swap_branchless)
				sort::swap_branchless_unconditional(*last, *mid);
			else
				sort::iter_swap(last, mid);

			if (pred(*mid, *first)) {
				if constexpr (use_swap_branchless)
					sort::swap_branchless_unconditional(*mid, *first);
				else
					sort::iter_swap(mid, first);
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
		using T                            = sort::iter_value_t<It>;
		constexpr bool use_swap_branchless = (std::is_arithmetic<T>::value || std::is_same<T, bool>::value) &&
											 std::is_same<sort::remove_cvref_t<decltype(*start)>, T>::value;
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
					if constexpr (use_swap_branchless) {
						sort::swap_branchless_unconditional(*_Plast, *_Gfirst);
					} else {
						sort::iter_swap(_Plast, _Gfirst);
					}
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
					// sort::swap_branchless_unconditional(*_Pfirst, *_Glast_prev);
					if constexpr (use_swap_branchless)
						sort::swap_branchless_unconditional(*_Pfirst, *_Glast_prev);
					else
						sort::iter_swap(_Pfirst, _Glast_prev);
				}
			}

			if (_Glast == start && _Gfirst == end) {
				return std::pair<It, It>(_Pfirst, _Plast);
			}

			if (_Glast == start) { // no room at bottom, rotate pivot upward
				if (_Plast != _Gfirst) {
					if constexpr (use_swap_branchless)
						sort::swap_branchless_unconditional(*_Pfirst, *_Plast);
					else
						sort::iter_swap(_Pfirst, _Plast);
				}

				++_Plast;

				if constexpr (use_swap_branchless)
					sort::swap_branchless_unconditional(*_Pfirst, *_Gfirst);
				else
					sort::iter_swap(_Pfirst, _Gfirst);

				++_Pfirst;
				++_Gfirst;
			} else if (_Gfirst == end) { // no room at top, rotate pivot downward
				if (--_Glast != --_Pfirst) {
					if constexpr (use_swap_branchless)
						sort::swap_branchless_unconditional(*_Glast, *_Pfirst);
					else
						sort::iter_swap(_Glast, _Pfirst);
				}
				if constexpr (use_swap_branchless)
					sort::swap_branchless_unconditional(*_Pfirst, *--_Plast);
				else
					sort::iter_swap(_Pfirst, --_Plast);
			} else {
				if constexpr (use_swap_branchless)
					sort::swap_branchless_unconditional(*_Gfirst, *--_Glast);
				else
					sort::iter_swap(_Gfirst, --_Glast);
				++_Gfirst;
			}
		}
	}

	template<typename It, typename Compare>
	constexpr void intro_sort(It first, It last, Compare comp, size_t heapthresh)
	{
		if constexpr (is_std_less<Compare>::value) {
			return sort::intro_sort(first, last, sort::less<>{}, heapthresh);
		} else if constexpr (is_std_greater<Compare>::value) {
			return sort::intro_sort(first, last, sort::greater<>{}, heapthresh);
		} else {
			using value_type = sort::iter_value_t<It>;
			for (;;) {
				size_t count = sort::distance(first, last);
				// TODO: find more cases where this makes sense to do
				if constexpr (::std::is_default_constructible<value_type>::value &&
								::std::is_same<typename ::std::iterator_traits<It>::iterator_category,
												::std::random_access_iterator_tag>::value) {
					if (count <= small_merge_sort_threshold) { // this performs better
						sort::small_merge_sort(first, last, comp);
						return;
					}
				} else {
					if (count <= 32) {
						sort::insertion_sort(first, last, comp);
						return;
					}
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
	template<typename It, typename Comp = sort::less<>> constexpr sorting_algorithms sorting_algorithm(It, It, Comp)
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

	template<typename It, typename Comp = sort::less<>> constexpr void sort(It start, It end, Comp comp = Comp{})
	{
		using value_type         = sort::remove_cvref_t<decltype(*std::declval<It>())>;
		using comparator_details = comparator_info<It, Comp>;

		if constexpr (std::is_same<Comp, std::less<value_type>>::value) {
			sort::sort(start, end, sort::less<value_type>{});
		} else if constexpr (std::is_same<Comp, std::less<>>::value) {
			sort::sort(start, end, sort::less<>{});
		} else if constexpr (std::is_same<Comp, std::greater<value_type>>::value) {
			sort::sort(start, end, sort::greater<value_type>{});
		} else if constexpr (std::is_same<Comp, std::greater<>>::value) {
			sort::sort(start, end, sort::greater<>{});
		} else if constexpr (comparator_details::is_partition) {
			// partition puts things that return true first...but counting sort should treat this as a value
			// so...we'll flip the extract function to keep the semantics the same as expected
			sort::reversed_partition(get_unwrapped(start), get_unwrapped(end), comp);
		} else if constexpr (std::is_integral<value_type>::value &&
							 (std::is_same<Comp, sort::less<>>::value ||
											 std::is_same<Comp, sort::less<value_type>>::value)) {
			// we're sorting integral data using < or >, use counting sort
			sort::counting_sort(start, end, identity_less_than<value_type>{});
		} else if constexpr (sort::is_convertible_to_integrals<value_type>::value &&
							 (std::is_same<Comp, sort::less<>>::value ||
											 std::is_same<Comp, sort::less<value_type>>::value)) {
			// we're sorting integral data using < or >, use counting sort
			sort::counting_sort(start, end, identity_less_than<value_type>{});
		} else if constexpr (std::is_integral<value_type>::value &&
							 (std::is_same<Comp, sort::greater<>>::value ||
											 std::is_same<Comp, sort::greater<value_type>>::value)) {
			sort::counting_sort(start, end, identity_greater_than<value_type>{});
		} else if constexpr (sort::is_convertible_to_integrals<value_type>::value &&
							 (std::is_same<Comp, sort::greater<>>::value ||
											 std::is_same<Comp, sort::greater<value_type>>::value)) {
			sort::counting_sort(start, end, identity_greater_than<value_type>{});
		} else if constexpr (comparator_details::is_keyed) {
			sort::counting_sort(start, end, comp);
		} else if constexpr (comparator_details::is_comparator) {
			// comparator sorts
			if constexpr (std::is_same<typename std::forward_iterator_tag,
										  typename std::iterator_traits<It>::iterator_category>::value ||
							std::is_same<typename std::bidirectional_iterator_tag,
											typename std::iterator_traits<It>::iterator_category>::value) {
				sort::counting_sort(start, end, comp);
			} else {
				static_assert(false, "WARNING! The behavior of this fallback does not match that of sort::sort! Remove "
									 "at your own discretion!");
				sort::intro_sort(sort::get_unwrapped(start), sort::get_unwrapped(end), comp, end - start);
			}
		} else {
			static_assert(false, "Provided callback function must transform iterators into keys or be a "
								 "comparison function");
		}
	}
} // namespace sort