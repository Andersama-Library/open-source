#pragma once

#include <array>
#include <cstdint>
#include <memory>
#include <cassert>
#include <version>
#include <stdexcept>
#include <type_traits>
#include <iterator>
#include <coroutine>
#include <iostream>
#if __cpp_lib_generator
#include <generator>
#endif
#if __cpp_lib_containers_ranges
#include <ranges>
#endif

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

namespace stable_array {
#if __cpp_lib_containers_ranges
	template<class R, class T>
	concept stable_array_container_compatible_range =
					std::ranges::input_range<R> && std::convertible_to<std::ranges::range_reference_t<R>, T>;
#endif

#if WIN32
	always_force_inline uint32_t bsr(uint64_t x)
	{
		unsigned long bsr_v;
		_BitScanReverse64(&bsr_v, x | 1ull);
		return bsr_v;
	};
#else
	always_force_inline uint32_t bsr(uint64_t x)
	{
		return (uint64_t)63 - __builtin_clz((uint64_t)x | 1ull);
	}
#endif

	always_force_inline constexpr uint32_t satsub32(uint32_t x, uint32_t y) noexcept
	{
		uint32_t res = x - y;
		res &= -(res <= x);
		return res;
	}

	namespace util_stable_array {
		template<class Alloc> constexpr void pocca(Alloc& lhs, const Alloc& rhs) noexcept
		{
			if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value) {
				lhs = rhs;
			}
		}

		template<class Alloc> constexpr void pocma(Alloc& lhs, Alloc& rhs) noexcept
		{ // (maybe) propagate on container move assignment
			if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
				lhs = std::move(rhs);
			}
		}

		template<class Alloc> constexpr void pocs(Alloc& lhs, Alloc& rhs) noexcept
		{
			if constexpr (std::allocator_traits<Alloc>::propagate_on_container_swap::value) {
				std::swap(lhs, rhs);
			} else {
				assert(lhs == rhs, "allocators incompatible for swap");
			}
		}

		template<class T, class... Args>
		constexpr void construct_in_place(T& _Obj, Args&&... _Args) noexcept(
						std::is_nothrow_constructible_v<T, Args...>)
		{
#ifdef __cplusplus
#if defined(_MSVC_LANG) && _MSVC_LANG > __cplusplus
#define _STL_LANG _MSVC_LANG
#else // ^^^ language mode is _MSVC_LANG / language mode is __cplusplus vvv
#define _STL_LANG __cplusplus
#endif // ^^^ language mode is larger of _MSVC_LANG and __cplusplus ^^^
#else  // ^^^ determine compiler's C++ mode / no C++ support vvv
#define _STL_LANG 0L
#endif // ^^^ no C++ support ^^^
#if _STL_LANG > 201703L
			if (std::is_constant_evaluated()) {
				std::construct_at(std::addressof(_Obj), std::forward<Args>(_Args)...);
			} else
#endif
			{
				::new (static_cast<void*>(std::addressof(_Obj))) T(std::forward<Args>(_Args)...);
			}
		}
	}; // namespace util_stable_array

	template<typename T, typename Alloc = std::allocator<T>> struct stable_array {
		static constexpr uint32_t initial_shift = 12; // must be > 1
		static_assert(initial_shift > 1, "The initial allocation must be a large enough power of 2");
		static constexpr uint32_t ptr_count     = 25; // 128GB //27; 512GB//29; 2.048TB //32; //32TB (2^32 bytes = 4GB)
		static constexpr uint32_t initial_alloc = 1 << initial_shift;
		static constexpr uint64_t max_count     = (1ull << (ptr_count + initial_shift));
		static constexpr uint64_t max_bytes     = max_count * sizeof(T);

private:
		uint64_t                  sz{};
		uint64_t                  cap{};
		std::array<T*, ptr_count> ptrs{};
		Alloc                     alloc{};

public:
		struct idxs {
			uint64_t blk_idx;
			uint64_t el_idx;

			constexpr idxs& inc(uint64_t& cap_at_blk)
			{
				++el_idx;
				// if (x == a) x = b
				uint64_t l = el_idx >= cap_at_blk;
				blk_idx += l;
				el_idx     = el_idx - (el_idx * l); // l ? 0 : el_idx;
				cap_at_blk = cap_at_blk << l;
				return *this;
			};

			constexpr idxs& dec(uint64_t& cap_at_blk)
			{
				uint8_t l  = el_idx == 0;
				cap_at_blk = cap_at_blk >> (l & blk_idx > 1);
				blk_idx -= l;
				el_idx = l ? cap_at_blk - 1 : el_idx - 1;
				return *this;
			}
		};

		Alloc get_allocator() const
		{
			return alloc;
		};
#if 1
		constexpr stable_array() noexcept(noexcept(Alloc())) : stable_array(Alloc()) {};

		constexpr explicit stable_array(const Alloc& alloc)
		{
			this->alloc = alloc;
		};
		constexpr explicit stable_array(size_t count, const Alloc& alloc = Alloc())
		{
			this->alloc = alloc;
			assign(count, T{});
		};

		constexpr stable_array(size_t count, const T& value, const Alloc& alloc = Alloc())
		{
			this->alloc = alloc;
			assign(count, value);
		};

		template<class InputIt> stable_array(InputIt first, InputIt last, const Alloc& alloc = Alloc())
		{
			this->alloc = alloc;
			assign(first, last);
		};

#if __cpp_lib_containers_ranges
		template<stable_array_container_compatible_range<T> R>
		constexpr stable_array(std::from_range_t, R&& rg, const Alloc& alloc = Alloc())
		{
			this->alloc = alloc;
			append_range(rg);
		}
#endif

		constexpr stable_array(const stable_array& other)
		{
			this->operator=(other);
		};

		constexpr stable_array(stable_array&& other)
		{
			this->operator=(other);
		};

		constexpr stable_array(const stable_array& other, const std::type_identity_t<Alloc>& alloc)
		{
			this->alloc = alloc;
			assign(other.begin(), other.end());
			// this->operator=(other);
		};

		constexpr stable_array(stable_array&& other, const std::type_identity_t<Alloc>& alloc)
		{
			this->alloc = alloc;
			assign(other.begin(), other.end());
			// this->operator=(other);
		};
		constexpr stable_array(std::initializer_list<T> init, const Alloc& alloc = Alloc())
		{
			this->alloc = alloc;
			assign(init.begin(), init.end());
		};
#endif

		static always_force_inline constexpr uint64_t capacity_for_blks(uint32_t i)
		{
			return (1ull << initial_shift) << i;
		}
		static always_force_inline constexpr uint64_t capacity_at_blk(uint32_t i)
		{
			return (1ull << initial_shift) << satsub32(i, 1);
		}
		static always_force_inline constexpr uint64_t capacity_below_blk(uint32_t i)
		{
			constexpr uint64_t mask = ~((1ull << initial_shift) - 1ull);
			return ((1ull << (initial_shift - 1)) << i) & mask;
		}

		static always_force_inline constexpr uint32_t blk(size_t idx)
		{
			uint32_t bsr_idx = bsr(idx); // bit index
			return satsub32(bsr_idx, initial_shift - (uint32_t)1);
		}

		static always_force_inline constexpr idxs where(size_t idx)
		{
			idxs     ret;
			uint32_t bsr_idx = bsr(idx);
			ret.blk_idx      = satsub32(bsr_idx, initial_shift - 1);

			uint64_t cap_below_idx = capacity_below_blk(ret.blk_idx);
			ret.el_idx             = idx - cap_below_idx;
			return ret;
		}

		static always_force_inline constexpr T* ptr_to_idx(size_t idx, T** ptrs)
		{
			uint32_t bsr_idx = bsr(idx); // bit index
			uint32_t blk_idx = satsub32(bsr_idx, initial_shift - (uint32_t)1);

			uint64_t cap_sum_below_idx = capacity_below_blk(blk_idx);
			uint64_t el_idx            = idx - cap_sum_below_idx;

			return ptrs[blk_idx] + el_idx;
		}

		struct iterator : public std::iterator<std::random_access_iterator_tag, T> {
			uint64_t idx{};
			T**      ptrs{};

	public:
			constexpr explicit iterator(uint64_t i = 0) : idx(i) {};
			constexpr iterator(uint64_t i, T** p) : idx(i), ptrs(p) {};

			constexpr iterator& operator++()
			{
				++idx;
				return *this;
			};
			constexpr iterator operator++(int)
			{
				iterator ret = *this;
				++(*this);
				return ret;
			};
			constexpr iterator& operator--()
			{
				--idx;
				return *this;
			};
			constexpr iterator operator--(int)
			{
				iterator ret = *this;
				--(*this);
				return ret;
			};

			constexpr iterator& operator+=(std::iterator<std::random_access_iterator_tag, T>::difference_type i)
			{
				idx += i;
				return *this;
			};
			constexpr iterator& operator-=(std::iterator<std::random_access_iterator_tag, T>::difference_type i)
			{
				idx += i;
				return *this;
			};
			constexpr iterator operator+(std::iterator<std::random_access_iterator_tag, T>::difference_type i)
			{
				iterator other{idx + i, ptrs};
				return other;
			};
			constexpr iterator operator-(std::iterator<std::random_access_iterator_tag, T>::difference_type i)
			{
				iterator other{idx - i, ptrs};
				return other;
			};

			constexpr int64_t operator-(iterator other) const
			{
				return (int64_t)idx - (int64_t)other.idx;
			};

			constexpr bool operator==(iterator other) const
			{
				return idx == other.idx && ptrs == other.ptrs;
			};
			constexpr bool operator!=(iterator other) const
			{
				return !(*this == other);
			};
			constexpr bool operator<(iterator other) const
			{
				return idx < other.idx && ptrs == other.ptrs;
			};
			constexpr bool operator>(iterator other) const
			{
				return idx > other.idx && ptrs == other.ptrs;
			};
			constexpr bool operator<=(iterator other) const
			{
				return idx <= other.idx && ptrs == other.ptrs;
			};
			constexpr bool operator>=(iterator other) const
			{
				return idx >= other.idx && ptrs == other.ptrs;
			};

			constexpr T& operator*() const
			{
				return *ptr_to_idx(idx, ptrs);
			};

			constexpr T& operator[](std::iterator<std::random_access_iterator_tag, T>::difference_type i) const
			{
				return *ptr_to_idx(idx + i, ptrs);
			};
		};

		using reverse_iterator       = std::reverse_iterator<iterator>;
		using const_iterator         = std::basic_const_iterator<iterator>;
		using const_reverse_iterator = std::basic_const_iterator<reverse_iterator>;
		using value_type             = T;

		constexpr size_t size()
		{
			return sz;
		}

		constexpr size_t capacity()
		{
			return cap;
		}

		constexpr size_t max_capacity() const
		{
			return max_count;
		}

		constexpr size_t max_size() const
		{
			return max_count;
		}

		constexpr bool empty() const
		{
			return sz == 0;
		}

		/* NOTE: Not like vector, we return a pointer to a pointer */
		constexpr T** data() const
		{
			return ptrs.data();
		}

		constexpr iterator begin()
		{
			return iterator(0, ptrs.data());
		}

		constexpr iterator end()
		{
			return iterator(sz, ptrs.data());
		}

		constexpr const_iterator cbegin()
		{
			return const_iterator(iterator(0, ptrs.data()));
		}

		constexpr const_iterator cend()
		{
			return const_iterator(iterator(sz, ptrs.data()));
		}

		constexpr reverse_iterator rbegin()
		{
			return reverse_iterator{iterator(sz, ptrs.data())};
		}

		constexpr reverse_iterator rend()
		{
			return reverse_iterator{iterator(0, ptrs.data())};
		}

		constexpr const_reverse_iterator crbegin()
		{
			return const_reverse_iterator{reverse_iterator{iterator(sz, ptrs.data())}};
		}

		constexpr const_reverse_iterator crend()
		{
			return const_reverse_iterator{reverse_iterator{iterator(0, ptrs.data())}};
		}

		template<typename... Args> constexpr T& emplace_back(Args... args)
		{
			uint32_t bsr_idx        = bsr(sz); // bit index
			uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
			uint64_t cap_sum_at_idx = 1ull << (initial_shift + blk_idx);

			constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
			uint64_t           cap_sum_below_idx = (cap_sum_at_idx >> 1ull) & mask;
			uint64_t           el_idx            = sz - cap_sum_below_idx;

			T*   ptr_blk         = ptrs[blk_idx];
			bool should_allocate = ptr_blk == 0;

			if (should_allocate) {
				uint64_t cap_for_idx = cap_sum_at_idx >> (1ull - satsub32((uint32_t)1, blk_idx));
				// cap_sum_at_idx >> (blk_idx > 0);
#if __cpp_lib_allocate_at_least
				std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
				ptrs[blk_idx] = ptr_blk = res.ptr;
#else
				ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
				cap = cap_sum_at_idx;
			}

			T* ptr = ptr_blk + el_idx;
			std::construct_at(ptr, std::forward<Args>(args)...);
			sz += 1;
			return *ptr;
		}

		constexpr T& push_back(T& v)
		{
			return emplace_back(std::forward<T&>(v));
		}

		constexpr T& push_back(T&& v)
		{
			return emplace_back(std::forward<T&&>(v));
		}

		constexpr T& operator[](size_t idx)
		{
#if _DEBUG
			if (idx >= sz)
				throw std::out_of_range("idx was out of range");
#endif
			return *ptr_to_idx(idx, ptrs.data());
		}

		constexpr const T& operator[](size_t idx) const
		{
#if _DEBUG
			if (idx >= sz)
				throw std::out_of_range("idx was out of range");
#endif
			return *ptr_to_idx(idx, (T**)ptrs.data());
		};

		constexpr T& at(size_t idx)
		{
			if (idx >= sz)
				throw std::out_of_range("idx was out of range");
			return *ptr_to_idx(idx, ptrs.data());
		}

		constexpr const T& at(size_t idx) const
		{
			if (idx >= sz)
				throw std::out_of_range("idx was out of range");
			return *ptr_to_idx(idx, (T**)ptrs.data());
		};

		constexpr void pop_back()
		{
			size_t idx = sz;
			if (idx) {
				T* ptr = ptr_to_idx(idx - 1, ptrs.data());
				std::destroy_at(ptr);
				sz -= 1;
			}
		}

		constexpr T& back()
		{
			return *ptr_to_idx(sz - 1, ptrs.data());
		}

		constexpr const T& back() const
		{
			return *ptr_to_idx(sz - 1, (T**)ptrs.data());
		}

		constexpr T& front()
		{
			return ptrs[0][0];
		}

		constexpr const T& front() const
		{
			return ptrs[0][0];
		}

		void assign(size_t count, const T& value)
		{
			idxs assigned = where(count);
			idxs current  = where(sz);

			uint64_t blk_idx = count < sz ? assigned.blk_idx : current.blk_idx;
			uint64_t el_idx  = count < sz ? assigned.el_idx : current.el_idx;
			// Overwrite values
			for (size_t p = 0; p < blk_idx; p++) {
				T*       ptr        = ptrs[p];
				uint64_t cap_at_blk = capacity_at_blk(p);
				for (size_t i = 0; i < cap_at_blk; i++) {
					ptr[i] = value;
				}
			}

			T* ptr = ptrs[blk_idx];
			for (size_t i = 0; i < el_idx; i++) {
				ptr[i] = value;
			}

			if (count < sz) {
				// shrinking, destroy range
				if constexpr (!std::is_trivially_destructible<T>::value) {
					auto ps = ptrs.data();
					erase(iterator{count, ps}, iterator{sz, ps});
				} else {
					sz = count;
				}
			} else if (assigned.blk_idx > current.blk_idx) {
				// finish last block
				uint64_t cap_at_blk = capacity_at_blk(blk_idx);

				bool should_allocate = !ptr;
				if (should_allocate) {
#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_at_blk)};
					ptrs[blk_idx] = ptr = res.ptr;
#else
					ptrs[blk_idx] = ptr = alloc.allocate(cap_at_blk);
#endif
				}

				for (size_t i = el_idx; i < cap_at_blk; i++) {
					std::construct_at(ptr + i, value);
				}
				// fill uninitialized
				for (size_t p = blk_idx; p < assigned.blk_idx; p++) {
					T*       ptr        = ptrs[p];
					uint64_t cap_at_blk = capacity_at_blk(p);

					bool should_allocate = !ptr;
					if (should_allocate) {
#if __cpp_lib_allocate_at_least
						std::allocation_result res{alloc.allocate_at_least(cap_at_blk)};
						ptrs[blk_idx] = ptr = res.ptr;
#else
						ptrs[blk_idx] = ptr = alloc.allocate(cap_at_blk);
#endif
					}

					for (size_t i = 0; i < cap_at_blk; i++) {
						std::construct_at(ptr + i, value);
					}
				}

				T* ptr = ptrs[assigned.blk_idx];

				should_allocate = !ptr;
				if (should_allocate) {
					uint64_t cap_at_blk = capacity_at_blk(blk_idx);
#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_at_blk)};
					ptrs[blk_idx] = ptr = res.ptr;
#else
					ptrs[blk_idx] = ptr = alloc.allocate(cap_at_blk);
#endif
				}

				for (size_t i = 0; i < assigned.el_idx; i++) {
					std::construct_at(ptr + i, value);
				}
				sz = count;
			} else {
				// finish last block
				for (size_t i = el_idx; i < assigned.el_idx; i++) {
					std::construct_at(ptr + i, value);
				}
				sz = count;
			}
		}

		void assign(std::initializer_list<T> ilist)
		{
			assign(ilist.begin(), ilist.end());
		}

		template<std::input_or_output_iterator It> void assign(It first, It last)
		{
			clear();
			auto b = first;
			while (b != last) {
				uint32_t bsr_idx        = bsr(sz); // bit index
				uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
				uint64_t cap_sum_at_idx = (1ull << initial_shift) << blk_idx;
				// 1ull << (initial_shift + blk_idx);

				constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
				uint64_t           cap_sum_below_idx = (cap_sum_at_idx >> 1ull) & mask;
				// uint64_t cap_sum_below_idx = (blk_idx > 0) * cap_sum_at_idx >> 1ull;
				uint64_t el_idx = sz - cap_sum_below_idx;
				// only check for allocations at critical points
				T*   ptr_blk         = ptrs[blk_idx];
				bool should_allocate = ptr_blk == 0;

				uint64_t cap_for_idx = capacity_at_blk(blk_idx);
				if (should_allocate) {
#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
					ptrs[blk_idx] = ptr_blk = res.ptr;
#else
					ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
					cap = cap_sum_at_idx;
				}

				// loop through chunk
				ptr_blk = ptrs[blk_idx];
				for (size_t i = el_idx; i < cap_for_idx && b != last; i++) {
					std::construct_at(ptr_blk + i, *b);
					++b;
					sz += 1;
				}
			}
		}

#if __cpp_lib_containers_ranges
		template<stable_array_container_compatible_range<T> R> constexpr void assign_range(R&& rg)
		{
			clear();
			append_range(rg);
		}
#endif

		constexpr stable_array& operator=(const stable_array& other)
		{
			if constexpr (std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value &&
							!std::allocator_traits<Alloc>::is_always_equal::value) {

				Alloc& _Al       = this->alloc;
				Alloc& _Right_al = other.alloc;
				if (_Al != _Right_al) {
					cleanup_allocations();
					util_stable_array::pocca(_Al, _Right_al);
					append(other.begin(), other.end());
				} else {
					clear();
					append(other.begin(), other.end());
				}
			} else {
				assign(other.begin(), other.end());
			}
			return *this;
		}

		constexpr void cleanup_allocations() noexcept
		{ // free all storage
			if constexpr (!std::is_trivially_destructible<T>::value) {
				clear();
			}
			for (size_t p = ptrs.size() - 1; p < ptrs.size(); --p) {
				T* ptr = ptrs[p];
				if (!ptr)
					continue;
				uint64_t cap_at_blk = capacity_at_blk(p);
				alloc.deallocate(ptr, cap_at_blk);
			}
			if constexpr (std::is_trivially_destructible<T>::value) {
				sz  = 0;
				cap = 0;
			}
		}

		constexpr stable_array& operator=(stable_array&& other) noexcept(
						std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value ||
						std::allocator_traits<Alloc>::is_always_equal::value)
		{ // noexcept(_Choose_pocma_v<_Alty> != _Pocma_values::_No_propagate_allocators)
			if (this == std::addressof(other)) {
				return *this;
			}

			Alloc& _Al       = this->alloc;
			Alloc& _Right_al = other.alloc;
			if constexpr (std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value &&
							!std::allocator_traits<Alloc>::is_always_equal::value) {
				if (_Al != _Right_al) {
					assign(other.begin(), other.end());
					return *this;
				}
			}
			// Destroy this container
			cleanup_allocations();
			// Copy contents of other to this container
			// Reset temporary container to nothing
			util_stable_array::pocma(_Al, _Right_al);
			sz        = other.sz;
			cap       = other.cap;
			other.sz  = 0;
			other.cap = 0;
			for (size_t i = 0; i < ptrs.size(); i++) {
				ptrs[i]       = other.ptrs[i];
				other.ptrs[i] = nullptr;
			}
			return *this;
		}

		constexpr stable_array& operator=(std::initializer_list<T> ilist)
		{
			assign(ilist.begin(), ilist.end());
		}

#if __cpp_concepts
		template<std::input_or_output_iterator It> constexpr void append(It first, It last)
		{
			auto b = first;
			while (b != last) {
				uint32_t bsr_idx        = bsr(sz); // bit index
				uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
				uint64_t cap_sum_at_idx = (1ull << initial_shift) << blk_idx; // 1ull << (initial_shift + blk_idx);

				constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
				uint64_t           cap_sum_below_idx = cap_sum_at_idx >> 1ull;
				if constexpr (initial_shift) {
					cap_sum_below_idx &= mask;
				} else {
					cap_sum_below_idx *= (uint64_t)(blk_idx > 0);
				}

				uint64_t el_idx = sz - cap_sum_below_idx;
				// only check for allocations at critical points
				T*   ptr_blk         = ptrs[blk_idx];
				bool should_allocate = ptr_blk == 0;

				if (should_allocate) {
					uint64_t cap_for_idx = cap_sum_at_idx >> (blk_idx > 0);
#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
					ptrs[blk_idx] = ptr_blk = res.ptr;
#else
					ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
					cap = cap_sum_at_idx;
				}

				// loop through chunk
				ptr_blk              = ptrs[blk_idx];
				uint64_t cap_for_idx = cap_sum_at_idx >> (blk_idx > 0);
				for (size_t i = el_idx; i < cap_for_idx && b != last; i++) {
					std::construct_at(ptr_blk + i, *b);
					++b;
					sz += 1;
				}
			}
		};
#else
		template<typename It> explicit constexpr void append(It first, It last)
		{
			auto b = first;
			while (b != last) {
				uint32_t bsr_idx           = bsr(sz); // bit index
				uint32_t blk_idx           = satsub32(bsr_idx, initial_shift - (uint32_t)1);
				uint64_t cap_sum_at_idx    = (1ull << initial_shift) << blk_idx; // 1ull << (initial_shift + blk_idx);
				uint64_t cap_sum_below_idx = (blk_idx > 0) * cap_sum_at_idx >> 1ull;
				uint64_t el_idx            = sz - cap_sum_below_idx;
				// only check for allocations at critical points
				T*       ptr_blk         = ptrs[blk_idx];
				bool     should_allocate = ptr_blk == 0;
				uint64_t cap_for_idx     = capacity_at_blk(blk_idx);

				if (should_allocate) {

#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
					ptrs[blk_idx] = ptr_blk = res.ptr;
#else
					ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
					cap = cap_sum_at_idx;
				}

				// loop through chunk
				ptr_blk = ptrs[blk_idx];
				for (size_t i = el_idx; i < cap_for_idx && b != last; i++) {
					std::construct_at(ptr_blk + i, *b);
					++b;
					sz += 1;
				}
			}
		};
#endif

		template<typename... Args> constexpr void emplace_back_n(size_t count, Args... args)
		{
			// auto b = first;
			size_t b = 0;
			while (b != count) {
				uint32_t bsr_idx        = bsr(sz); // bit index
				uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
				uint64_t cap_sum_at_idx = (1ull << initial_shift) << blk_idx;

				constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
				uint64_t           cap_sum_below_idx = cap_sum_at_idx >> 1ull;
				if constexpr (initial_shift) {
					cap_sum_below_idx &= mask;
				} else {
					cap_sum_below_idx *= (uint64_t)(blk_idx > 0);
				}
				uint64_t el_idx = sz - cap_sum_below_idx;
				// only check for allocations at critical points
				T*       ptr_blk         = ptrs[blk_idx];
				bool     should_allocate = ptr_blk == 0;
				uint64_t cap_for_idx     = capacity_at_blk(blk_idx);

				if (should_allocate) {

#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
					ptrs[blk_idx] = ptr_blk = res.ptr;
#else
					ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
					cap = cap_sum_at_idx;
				}

				// loop through chunk
				ptr_blk = ptrs[blk_idx];
				for (size_t i = el_idx; i < cap_for_idx && b != count; i++) {
					std::construct_at(ptr_blk + i, std::forward<Args>(args)...);
					++b;
					sz += 1;
				}
			}
		};

		constexpr void append(size_t count, value_type& value)
		{
			emplace_back_n(count, value);
		};

		constexpr iterator erase(const const_iterator pos)
		{
			size_t idx = pos.base().idx;
			if (idx < sz) {
				T* ptr = ptr_to_idx(idx, (T**)ptrs.data());
				std::destroy_at(ptr);
				sz -= 1;
				T** ps = (T**)ptrs.data();
				for (size_t i = idx; i < sz; i++) {
					*ptr_to_idx(idx, ps) = std::move(*ptr_to_idx(idx + 1, ps));
				}
			}
			return iterator{idx, ptrs.data()};
		}

		constexpr iterator erase(const const_iterator first, const const_iterator last)
		{
			size_t fidx = first.base().idx;
			size_t lidx = last.base().idx;
			if (fidx == lidx) {
				return iterator{lidx, ptrs.data()};
			}
			if (fidx < sz) {
				idxs f = where(fidx);
				lidx   = lidx < sz ? lidx : sz;

				idxs   l = where(lidx);
				size_t d = lidx - fidx;

				if (f.blk_idx == l.blk_idx) {
					T* ptr = ptrs[f.blk_idx];
					for (size_t i = f.el_idx; i < l.el_idx; i++) {
						std::destroy_at(ptr + i);
					}
				} else if (f.blk_idx < l.blk_idx) {
					T*       ptr         = ptrs[f.blk_idx];
					uint64_t cap_for_idx = capacity_at_blk(f.blk_idx);
					for (size_t i = f.el_idx; i < cap_for_idx; i++) {
						std::destroy_at(ptr + i);
					}

					for (size_t p = f.blk_idx + 1; p < l.blk_idx; p++) {
						T*       ptr         = ptrs[p];
						uint64_t cap_for_idx = capacity_at_blk(p);
						for (size_t i = 0; i < cap_for_idx; i++) {
							std::destroy_at(ptr + i);
						}
					}

					ptr = ptrs[l.blk_idx];
					for (size_t i = 0; i < l.el_idx; i++) {
						std::destroy_at(ptr + i);
					}
				} else {
					return iterator{fidx, ptrs.data()};
				}

				T** ps = (T**)ptrs.data();
				sz -= d;
				for (size_t i = fidx; i < sz; i++) {
					*ptr_to_idx(i, ps) = std::move(*ptr_to_idx(i + d, ps));
				}
			}
			return iterator{fidx, ptrs.data()};
		}

		constexpr void resize(size_t count)
		{
			if (count < sz) {
				T** ps = (T**)ptrs.data();
				erase(const_iterator{iterator{count, ps}}, const_iterator{iterator{sz, ps}});
			} else if (count > sz) {
				emplace_back_n(count - sz);
			}
		}

		constexpr void resize(size_t count, const value_type& value)
		{
			if (count < sz) {
				T** ps = (T**)ptrs.data();
				erase(const_iterator{iterator{count, ps}}, const_iterator{iterator{sz, ps}});
			} else if (count > sz) {
				append(count - sz, value);
			}
		}

#if __cpp_lib_containers_ranges
		template<stable_array_container_compatible_range<T> R> constexpr void append_range(R&& rg)
		{
			auto b = rg.begin();
			auto e = rg.end();
			while (b != e) {
				uint32_t bsr_idx        = bsr(sz); // bit index
				uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
				uint64_t cap_sum_at_idx = (1ull << initial_shift) + blk_idx;

				constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
				uint64_t           cap_sum_below_idx = cap_sum_at_idx >> 1ull;
				if constexpr (initial_shift) {
					cap_sum_below_idx &= mask;
				} else {
					cap_sum_below_idx *= (uint64_t)(blk_idx > 0);
				}
				uint64_t el_idx = sz - cap_sum_below_idx;
				// only check for allocations at critical points
				T*       ptr_blk         = ptrs[blk_idx];
				bool     should_allocate = ptr_blk == 0;
				uint64_t cap_for_idx     = capacity_at_blk(blk_idx);

				if (should_allocate) {

#if __cpp_lib_allocate_at_least
					std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
					ptrs[blk_idx] = ptr_blk = res.ptr;
#else
					ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
					cap = cap_sum_at_idx;
				}

				// loop through chunk
				ptr_blk = ptrs[blk_idx];
				for (size_t i = el_idx; i < cap_for_idx && b != e; i++) {
					std::construct_at(ptr_blk + i, *b);
					++b;
					sz += 1;
				}
			}
		};
#endif

		template<class... Args> iterator emplace(const_iterator pos, Args&&... args)
		{
			uint32_t bsr_idx        = bsr(sz); // bit index
			uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
			uint64_t cap_sum_at_idx = (1ull << initial_shift) + blk_idx;

			constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
			uint64_t           cap_sum_below_idx = cap_sum_at_idx >> 1ull;
			if constexpr (initial_shift) {
				cap_sum_below_idx &= mask;
			} else {
				cap_sum_below_idx *= (uint64_t)(blk_idx > 0);
			}
			uint64_t el_idx = sz - cap_sum_below_idx;

			T*   ptr_blk         = ptrs[blk_idx];
			bool should_allocate = ptr_blk == 0;

			if (should_allocate) {
				uint64_t cap_for_idx = cap_sum_at_idx >> (uint8_t)(blk_idx > 0);
#if __cpp_lib_allocate_at_least
				std::allocation_result res{alloc.allocate_at_least(cap_for_idx)};
				ptrs[blk_idx] = ptr_blk = res.ptr;
#else
				ptrs[blk_idx] = ptr_blk = alloc.allocate(cap_for_idx);
#endif
			}

			cap = cap_sum_at_idx;
			// Shift all right by 1
			size_t idx = pos.base().idx;
			T**    ps  = (T**)ptrs.data();
			for (size_t i = sz; i > idx; i--) {
				*ptr_to_idx(i, ps) = std::move(*ptr_to_idx(i - 1, ps));
			}
			std::construct_at(ptr_to_idx(idx, ps), std::forward<Args>(args)...);
			sz += 1;

			return iterator{idx, ps};
		}

		constexpr iterator insert(const_iterator pos, const T& value)
		{
			return emplace(pos, value);
		}

		constexpr iterator insert(const_iterator pos, T&& value)
		{
			return emplace(pos, value);
		}

		struct handle_type {
			std::array<uint64_t, 2> idxs;
			static constexpr size_t depth = 2;
		};

		handle_type handle(size_t i)
		{
			auto w = where(i);
			return handle_type{w.blk_idx, w.el_idx};
		};

		template<typename CB> void for_each_ungaurded(handle_type first, handle_type last, CB cb)
		{
			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			for (size_t p = f0; p <= l0; p++) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = s; i < e; i++) {
					cb(*(ptr + i));
				}
			}
		}

		template<typename CB> void for_each_ungaurded_r(handle_type last, handle_type first, CB cb)
		{
			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			for (size_t p = f0; p >= l0 && p <= f0; --p) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = e - 1; i < e && i >= s; --i) {
					cb(*(ptr + i));
				}
			}
		}

		template<typename CB> void for_each(handle_type first, handle_type last, CB cb)
		{
			handle_type s = handle(sz);
			if (s.idxs[0] < last.idxs[0] || (s.idxs[0] == last.idxs[0] && s.idxs[0] < last.idxs[0]))
				last = s;

			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			for (size_t p = f0; p <= l0; p++) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = s; i < e; i++) {
					cb(*(ptr + i));
				}
			}
		}

		template<typename CB> void for_each_r(handle_type last, handle_type first, CB cb)
		{
			handle_type s = handle(sz);
			if (s.idxs[0] < last.idxs[0] || (s.idxs[0] == last.idxs[0] && s.idxs[0] < last.idxs[0]))
				last = s;

			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			for (size_t p = f0; p >= l0 && p <= f0; --p) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = e - 1; i < e && i >= s; --i) {
					cb(*(ptr + i));
				}
			}
		}

		template<typename CB> void for_each(handle_type first, handle_type last, handle_type next, CB cb)
		{
			handle_type s = handle(sz);
			if (s.idxs[0] < last.idxs[0] || (s.idxs[0] == last.idxs[0] && s.idxs[0] < last.idxs[0]))
				last = s;

			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			size_t n0 = next.idxs[0];
			size_t n1 = next.idxs[1];
			for (size_t p = f0; p <= l0; p++) {
				size_t s    = p == f0 ? f1 : 0;
				size_t e    = p == l0 ? l1 : capacity_at_blk(p);
				size_t ce   = capacity_at_blk(n0);
				T*     ptr  = ptrs[p];
				T*     nptr = ptrs[n0];
				for (size_t i = s; i < e; i++) {
					cb(*(ptr + i), *(ptrs[n0] + n1));

					n1 += 1;
					uint8_t next_blk = n1 >= ce;
					n0               = next_blk ? n0 + 1 : n0;
					n1               = next_blk ? 0 : n1;
					ce               = ce << next_blk;
				}
			}
		}

		template<typename CB> void for_each_r(handle_type last, handle_type first, handle_type next_last, CB cb)
		{
			handle_type s = handle(sz);
			if (s.idxs[0] < last.idxs[0] || (s.idxs[0] == last.idxs[0] && s.idxs[0] < last.idxs[0]))
				last = s;

			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			size_t n0 = next_last.idxs[0];
			size_t n1 = next_last.idxs[1];
			for (size_t p = f0; p <= l0; p++) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				size_t ce  = capacity_at_blk(n0);
				T*     ptr = ptrs[p];
				// T*     nptr = ptrs[n0];
				for (size_t i = s; i < e; i++) {
					cb(*(ptr + i), *(ptrs[n0] + n1));
					uint8_t next_blk = n1 == 0;
					// n1 -= 1;
					// uint8_t next_blk = n1 >= ce;
					n0 = next_blk ? n0 - 1 : n0;
					ce = capacity_at_blk(n0);
					n1 = next_blk ? ce - 1 : n1 - 1;
				}
			}
		}

		constexpr iterator insert(const_iterator pos, size_t count, const T& value)
		{
			// do allocations up front
			reserve(sz + count);

			size_t idx = pos.base().idx;
			T**    ps  = (T**)ptrs.data();

			idxs   fw = where(count - idx);
			idxs   lw = where(sz + count);
			size_t f0 = fw.blk_idx;
			size_t f1 = fw.el_idx;
			size_t l0 = lw.blk_idx;
			size_t l1 = lw.el_idx;
			//[idx,sz)
			//[sz+count-(sz-idx),sz+count]
			size_t w = sz - 1;
			for (size_t p = l0; p >= f0 && p <= l0; --p) {
				size_t s = p == f0 ? f1 : 0;
				size_t e = p == l0 ? l1 : capacity_at_blk(p);
				// size_t d   = e - s;
				T* ptr = ptrs[p];
				for (size_t i = e - 1; i < e && i >= s; --i) {
					*(ptr + i) = std::move(*ptr_to_idx(w, ps));
					--w;
				}
			}

			fw = where(idx);
			lw = where(idx + count);
			f0 = fw.blk_idx;
			f1 = fw.el_idx;
			l0 = lw.blk_idx;
			l1 = lw.el_idx;

			for (size_t p = f0; p <= l0; p++) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = s; i < e; i++) {
					std::construct_at(ptr + i, value);
				}
			}

			// move elements out of way
			/*
			for (size_t i = sz + count; i > idx + (count - 1); i--) {
				*ptr_to_idx(i, ps) = std::move(*ptr_to_idx(i - count, ps));
			}
			// construct elements in place
			for (size_t i = idx; i < idx + count; i++) {
				std::construct_at(ptr_to_idx(i, ps), value);
			}
			*/
			sz += count;
			return iterator{idx, ps};
		}

		template<class It> constexpr iterator insert(const_iterator pos, It first, It last)
		{
			// do allocations up front
			size_t count = std::distance(first, last);
			reserve(sz + count);

			size_t idx = pos.base().idx;
			T**    ps  = (T**)ptrs.data();

			idxs   fw = where(count - idx);
			idxs   lw = where(sz + count);
			size_t f0 = fw.blk_idx;
			size_t f1 = fw.el_idx;
			size_t l0 = lw.blk_idx;
			size_t l1 = lw.el_idx;
			//[idx,sz)
			//[sz+count-(sz-idx),sz+count]
			size_t w = sz - 1;
			for (size_t p = l0; p >= f0 && p <= l0; --p) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = e - 1; i < e && i >= s; --i) {
					*(ptr + i) = std::move(*ptr_to_idx(w, ps));
					//*ptr_to_idx(w, ps) = std::move(*(ptr + i));
					--w;
				}
			}

			fw = where(idx);
			lw = where(idx + count);
			f0 = fw.blk_idx;
			f1 = fw.el_idx;
			l0 = lw.blk_idx;
			l1 = lw.el_idx;

			for (size_t p = f0; p <= l0; p++) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = s; i < e; i++) {
					std::construct_at(ptr + i, *first);
					++first;
				}
			}

			sz += count;
			return iterator{idx, ps};
		}

		template<class It> constexpr iterator insert_basic(const_iterator pos, It first, It last)
		{
			// do allocations up front
			size_t count = std::distance(first, last);
			reserve(sz + count);

			size_t idx = pos.base().idx;
			T**    ps  = (T**)ptrs.data();

			// move elements out of way
			for (size_t i = sz + count; i > idx + (count - 1); i--) {
				*ptr_to_idx(i, ps) = std::move(*ptr_to_idx(i - count, ps));
			}

			for (size_t i = idx; i < idx + count; i++) {
				std::construct_at(ptr_to_idx(i, ps), *first);
				++first;
			}
			sz += count;
			return iterator{idx, ps};
		}

		constexpr iterator insert(const_iterator pos, std::initializer_list<T> ilist)
		{
			return insert(pos, ilist.begin(), ilist.end());
		}

		constexpr void clear()
		{
#ifdef _DEBUG
			constexpr bool debug_mode = true;
#else
			constexpr bool debug_mode = false;
#endif
			if constexpr (std::is_trivially_destructible<T>::value && !debug_mode) {
				sz = 0;
			} else {
				if (sz) {
					size_t   idx            = sz;
					uint32_t bsr_idx        = bsr(sz); // bit index
					uint32_t blk_idx        = satsub32(bsr_idx, initial_shift - (uint32_t)1);
					uint64_t cap_sum_at_idx = (1ull << initial_shift) << blk_idx;

					constexpr uint64_t mask              = ~((1ull << initial_shift) - 1ull);
					uint64_t           cap_sum_below_idx = cap_sum_at_idx >> 1ull;
					if constexpr (initial_shift) {
						cap_sum_below_idx &= mask;
					} else {
						cap_sum_below_idx *= (uint64_t)(blk_idx > 0);
					}
					uint64_t el_idx = idx - cap_sum_below_idx;

					T* ptr = ptrs[blk_idx];
					for (size_t i = el_idx - 1; i < el_idx; --i) {
						std::destroy_at(ptr + i);
					}

					for (size_t p = blk_idx - 1; p < blk_idx; --p) {
						T*       ptr         = ptrs[p];
						uint64_t cap_for_idx = capacity_at_blk(p);
						for (size_t i = cap_for_idx - 1; i < cap_for_idx; --i) {
							std::destroy_at(ptr + i);
						}
					}

					sz = 0;
				}
			}
		}

		constexpr void reserve(size_t new_cap)
		{
			if (cap >= new_cap)
				return;

			if (new_cap >= max_size())
				throw std::length_error("cannot reserve this amount of data");

			for (size_t p = 0; p < ptrs.size(); p++) {
				T*       ptr            = ptrs[p];
				uint64_t cap_sum_at_idx = (1ull << initial_shift) << p;
				if (!ptr) {
					uint64_t cap_for_idx = cap_sum_at_idx >> (p > 0);
					ptrs[p]              = alloc.allocate(cap_for_idx);
					cap                  = cap_sum_at_idx;
				}
				if (cap_sum_at_idx >= new_cap)
					break;
			}
		}

		constexpr void shrink_to_fit()
		{
			for (size_t p = ptrs.size() - 1; p > 0; --p) {
				T*       ptr            = ptrs[p];
				uint64_t cap_sum_at_idx = (1ull << initial_shift) + p;
				uint64_t cap_for_idx    = cap_sum_at_idx >> 1; //(p > 0);
				if (ptr && sz <= cap_for_idx) {
					alloc.deallocate(ptr, cap_for_idx);
					ptrs[p] = nullptr;
					cap     = cap_for_idx;
				}
			}

			if (sz == 0) {
				uint64_t cap_sum_at_idx = 1ull << initial_shift;
				alloc.deallocate(ptrs[0], cap_sum_at_idx);
				ptrs[0] = nullptr;
				cap     = 0;
			}
		}

#if __cpp_lib_generator
		std::generator<T&> iterate(handle_type first, handle_type last)
		{
			// prevent out of bounds access
			handle_type s = handle(sz);
			if (s.idxs[0] < last.idxs[0] || (s.idxs[0] == last.idxs[0] && s.idxs[0] < last.idxs[0]))
				last = s;

			size_t f0 = first.idxs[0];
			size_t f1 = first.idxs[1];
			size_t l0 = last.idxs[0];
			size_t l1 = last.idxs[1];
			for (size_t p = f0; p <= l0; p++) {
				size_t s   = p == f0 ? f1 : 0;
				size_t e   = p == l0 ? l1 : capacity_at_blk(p);
				T*     ptr = ptrs[p];
				for (size_t i = 0; i < e; i++) {
					co_yield *(ptr + i);
				}
			}
		}
		std::generator<T&> iterate()
		{
			return iterate(handle(0), handle(sz));
		}

#endif
		template<class T, class Alloc> constexpr int operator<=>(const stable_array<T, Alloc>& rhs)
		{
			T** ps = (T**)ptrs.data();
			for (size_t i = 0; i < sz && i < rhs.size(); i++) {
				auto c = *ptr_to_idx(i, ps) <=> rhs[i];
				if (c != 0)
					return c;
			}
			return 0;
		}

		constexpr friend void swap(stable_array<T>& lhs, stable_array<T>& rhs) noexcept(
						std::is_nothrow_move_constructible<T>::value && std::is_nothrow_move_assignable<T>::value)
		{
			std::swap(lhs.sz, rhs.sz);
			std::swap(lhs.cap, rhs.cap);
			std::swap(lhs.alloc, rhs.alloc);
			for (size_t p = 0; p < ptrs.size(); p++) {
				auto ptr    = lhs.ptrs[p];
				lhs.ptrs[p] = rhs.ptrs[p];
				rhs.ptrs[p] = ptr;
			}
		}

		~stable_array()
		{
			cleanup_allocations();
		}
	};

} // namespace stable_array