#pragma once
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

#include <vector>
#include <memory>
#include <type_traits>
#include <initializer_list>
#include <iterator>
#if defined __has_include
#if __has_include(<memory_resource>)
#include <memory_resource>
#endif
#if __has_include(<ranges>)
#include <ranges>
#endif
#if __has_include(<functional>)
#include <functional>
#endif
#endif
#include <cassert>
#include <algorithm>

namespace circular_buffer {

	namespace details {
		template<typename T> constexpr void destroy_at(T* const ptr)
		{
			if constexpr (!::std::is_trivially_destructible_v<T>)
			ptr->~T();
		}

		template<typename Iterator> constexpr void destroy(Iterator first, Iterator last)
		{
			if constexpr (!::std::is_trivially_destructible_v<typename ::std::iterator_traits<Iterator>::value_type>) {
				for (; first != last; ++first)
				destroy_at(::std::addressof(*first));
			}
		}

		// ripped from llvm libcxx
		struct default_init_tag {};
		struct value_init_tag {};
		struct zero_then_variadic_args_t {};
		struct one_then_variadic_args_t {};

		// propagate on container move assignment
		template<typename Alloc> constexpr void pocma(Alloc& left, Alloc& right) noexcept
		{
			if constexpr (::std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value) {
				left = ::std::move(right);
			}
		}

		template<typename Alloc> constexpr void pocca(Alloc& left, Alloc& right) noexcept
		{
			if constexpr (::std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value) {
				left = right;
			}
		}

		template<typename Alloc> constexpr bool should_pocma(Alloc&) noexcept
		{
			return ::std::allocator_traits<Alloc>::propagate_on_container_move_assignment::value;
		}

		template<typename Alloc> constexpr bool should_pocca(Alloc&) noexcept
		{
			return ::std::allocator_traits<Alloc>::propagate_on_container_copy_assignment::value;
		}

		template<typename T, bool> struct dependent_type : public T {};

		// can optimize Ty1 away (empty base class optimization)
		template<typename Ty1, typename Ty2, bool emptyBaseClass = ::std::is_empty_v<Ty1> && !::std::is_final_v<Ty1>>
		struct compressed_pair final : private Ty1 {
			Ty2 _value2;

			template<class... Args1, class... Args2, size_t... Idxs1, size_t... Idxs2>
			constexpr explicit compressed_pair(std::tuple<Args1...> first_args, std::tuple<Args2...> second_args,
											   std::index_sequence<Idxs1...>, std::index_sequence<Idxs2...>)
			: Ty1(::std::forward<Args1>(::std::get<Idxs1>(first_args))...),
			  _value2(::std::forward<Args2>(::std::get<Idxs2>(second_args))...)
			{
			}

			template<class... Args1, class... Args2>
			constexpr explicit compressed_pair(::std::piecewise_construct_t t, std::tuple<Args1...> first_args,
											   std::tuple<Args2...> second_args)
			: compressed_pair(first_args, second_args, std::make_index_sequence<sizeof...(Args1)>(),
							  std::make_index_sequence<sizeof...(Args2)>())
			{
			}

			template<class... Args>
			constexpr explicit compressed_pair(zero_then_variadic_args_t, Args&&... args) noexcept(
				::std::conjunction_v<::std::is_nothrow_default_constructible<Ty1>,
				::std::is_nothrow_constructible<Ty2, Args...>>)
			: Ty1(), _value2(::std::forward<Args>(args)...){};

			template<class Arg, class... Args>
			constexpr compressed_pair(one_then_variadic_args_t, Arg&& arg, Args&&... args) noexcept(
				::std::conjunction_v<::std::is_nothrow_default_constructible<Ty1>,
				::std::is_nothrow_constructible<Ty2, Args...>>)
			: Ty1(::std::forward<Arg>(arg)), _value2(::std::forward<Args>(args)...){};

			constexpr Ty1& first() noexcept
			{
				return *this;
			}
			constexpr const Ty1& first() const noexcept
			{
				return *this;
			}
			constexpr Ty2& second() noexcept
			{
				return _value2;
			}
			constexpr const Ty2& second() const noexcept
			{
				return _value2;
			}
		};

		template<typename Ty1, typename Ty2> struct compressed_pair<Ty1, Ty2, false> final {
			Ty1 _value1;
			Ty2 _value2;

			template<class... Args1, class... Args2, size_t... Idxs1, size_t... Idxs2>
			constexpr explicit compressed_pair(std::tuple<Args1...> first_args, std::tuple<Args2...> second_args,
											   std::index_sequence<Idxs1...>, std::index_sequence<Idxs2...>)
			: _value1(::std::forward<Args1>(::std::get<Idxs1>(first_args))...),
			  _value2(::std::forward<Args2>(::std::get<Idxs2>(second_args))...)
			{
			}

			template<class... Args1, class... Args2>
			constexpr explicit compressed_pair(::std::piecewise_construct_t t, std::tuple<Args1...> first_args,
											   std::tuple<Args2...> second_args)
			: compressed_pair(first_args, second_args, std::make_index_sequence<sizeof...(Args1)>(),
							  std::make_index_sequence<sizeof...(Args2)>())
			{
			}

			template<class... Args>
			constexpr explicit compressed_pair(zero_then_variadic_args_t, Args&&... args) noexcept(
				::std::conjunction_v<::std::is_nothrow_default_constructible<Ty1>,
				::std::is_nothrow_constructible<Ty2, Args...>>)
			: _value1(), _value2(::std::forward<Args>(args)...){};

			template<class Arg, class... Args>
			constexpr compressed_pair(one_then_variadic_args_t, Arg&& arg, Args&&... args) noexcept(
				::std::conjunction_v<::std::is_nothrow_default_constructible<Ty1>,
				::std::is_nothrow_constructible<Ty2, Args...>>)
			: _value1(::std::forward<Arg>(arg)), _value2(::std::forward<Args>(args)...){};

			constexpr Ty1& first() noexcept
			{
				return _value1;
			}
			constexpr const Ty1& first() const noexcept
			{
				return _value1;
			}
			constexpr Ty2& second() noexcept
			{
				return _value2;
			}
			constexpr const Ty2& second() const noexcept
			{
				return _value2;
			}
		};

		template<class T> using has_begin = decltype(std::declval<T&>().begin());

		template<class T> using has_end = decltype(std::declval<T&>().end());

		template<class T> using has_cbegin = decltype(std::declval<T&>().cbegin());

		template<class T> using has_cend = decltype(std::declval<T&>().cend());
	} // namespace details

	struct default_expansion_policy {
		[[nodiscard]] constexpr size_t grow_capacity(size_t size, size_t capacity, size_t required_capacity) noexcept
		{
			return required_capacity;
		}
	};

	template<size_t N> struct geometric_int_expansion_policy {
		[[nodiscard]] constexpr size_t grow_capacity(size_t size, size_t capacity, size_t required_capacity) noexcept
		{
			const size_t expanded_capacity = (capacity ? capacity : 1) * N;
			return (expanded_capacity < required_capacity ? required_capacity : expanded_capacity);
		}
	};

	template<double N> struct geometric_double_expansion_policy {
		[[nodiscard]] constexpr size_t grow_capacity(size_t size, size_t capacity, size_t required_capacity) noexcept
		{
			const size_t expanded_capacity = (capacity ? capacity : 1) * N;
			return (expanded_capacity < required_capacity ? required_capacity : expanded_capacity);
		}
	};

	// TODO: implement a strictly power of 2 capacity circular_buffer to avoid % operations
	template<typename T, typename Allocator = std::allocator<T>> class circular_buffer {
	public:
		using container_type  = std::vector<T, Allocator>;
		using value_type      = container_type::value_type;
		using allocator_type  = container_type::allocator_type;
		using size_type       = container_type::size_type;
		using difference_type = container_type::difference_type;
		using reference       = container_type::reference;
		using const_reference = container_type::const_reference;
		using pointer         = container_type::pointer;
		using const_pointer   = container_type::const_pointer;
		// using iterator_category = typename ::std::random_access_iterator_tag;

		struct random_access_it : public ::std::random_access_iterator_tag {
			using iterator_category = typename ::std::random_access_iterator_tag;

			using value_type      = value_type;
			using difference_type = difference_type;
			using pointer         = pointer;
			using reference       = reference;

			pointer data_ptr  = {};
			size_t  read_idx  = {};
			size_t  write_idx = {};
			size_t  capacity  = {};

			constexpr random_access_it() = default;
			constexpr random_access_it(pointer p, size_t r, size_t w, size_t c) : data_ptr{p}, write_idx{w}
			{
				capacity = (c ? c : c + 1);
				// determine the "real" read_idx
				read_idx = r % capacity;
			};

			constexpr value_type& operator*()
			{
				return data_ptr[read_idx];
			};

			constexpr value_type& operator*() const
			{
				return data_ptr[read_idx];
			};

			constexpr random_access_it& operator++() noexcept
			{
				// if we're right at the edge, loop back around to 0
				read_idx += 1 + ((read_idx == (capacity - 1)) * -capacity);
				write_idx++;
				return *this;
			};

			constexpr random_access_it& operator++() const noexcept
			{
				read_idx += 1 + ((read_idx == (capacity - 1)) * -capacity);
				write_idx++;
				return *this;
			};

			constexpr random_access_it operator++(int) noexcept
			{
				random_access_it tmp = *this;
				++*this;
				return tmp;
			};

			constexpr random_access_it operator++(int) const noexcept
			{
				random_access_it tmp = *this;
				++*this;
				return tmp;
			};

			constexpr random_access_it& operator--() noexcept
			{
				// if we're right at the edge, loop back around to capacity-1
				read_idx -= 1 + ((read_idx == 0) * -capacity);
				write_idx--;
				return *this;
			};

			constexpr random_access_it& operator--() const noexcept
			{
				// if we're right at the edge, loop back around to capacity-1
				read_idx -= 1 + ((read_idx == 0) * -capacity);
				write_idx--;
				return *this;
			};

			constexpr random_access_it operator--(int) noexcept
			{
				random_access_it tmp = *this;
				--*this;
				return tmp;
			};

			constexpr random_access_it operator--(int) const noexcept
			{
				random_access_it tmp = *this;
				--*this;
				return tmp;
			};

			constexpr difference_type operator-(const random_access_it& other) const noexcept
			{
				assert((other.data_ptr == data_ptr) && (other.capacity == capacity) &&
					   "Must be an iterator to the same container's contents");
				return static_cast<difference_type>(write_idx - other.write_idx);
			};

			constexpr random_access_it& operator+=(difference_type n) noexcept
			{
				size_t pos_idx = ((read_idx + n) % capacity);
				size_t neg_idx = (((read_idx + capacity) - ((-n) % capacity))) % capacity;
				read_idx       = (n >= 0) ? pos_idx : neg_idx;
				write_idx += n;
				return *this;
			};

			constexpr random_access_it& operator+=(difference_type n) const noexcept
			{
				size_t pos_idx = ((read_idx + n) % capacity);
				size_t neg_idx = (((read_idx + capacity) - ((-n) % capacity))) % capacity;
				read_idx       = (n >= 0) ? pos_idx : neg_idx;
				write_idx += n;
				return *this;
			};

			constexpr random_access_it operator+(difference_type n) noexcept
			{
				random_access_it bump = *this;
				return bump += n;
			};

			constexpr random_access_it operator+(difference_type n) const noexcept
			{
				random_access_it bump = *this;
				return bump += n;
			};

			constexpr random_access_it operator-(difference_type n) noexcept
			{
				random_access_it bump = *this;
				return bump -= n;
			};

			constexpr random_access_it operator-(difference_type n) const noexcept
			{
				random_access_it bump = *this;
				return bump -= n;
			};

			constexpr random_access_it& operator-=(difference_type n) noexcept
			{
				*this += -n;
				return *this;
			};

			constexpr random_access_it& operator-=(difference_type n) const noexcept
			{
				*this += -n;
				return *this;
			};

			constexpr reference operator[](size_t n)
			{
				return data_ptr[(read_idx + n) % capacity];
			};

			constexpr reference operator[](size_t n) const
			{
				return data_ptr[(read_idx + n) % capacity];
			};

			constexpr pointer operator->() noexcept
			{
				return data_ptr + (read_idx % capacity);
			};

			constexpr pointer operator->() const noexcept
			{
				return data_ptr + (read_idx % capacity);
			};

			constexpr bool operator<(random_access_it other) const noexcept
			{
				return (data_ptr == other.data_ptr) & (capacity == other.capacity) & (write_idx < other.write_idx);
			};

			constexpr bool operator>(random_access_it other) const noexcept
			{
				return (data_ptr == other.data_ptr) & (capacity == other.capacity) & (write_idx > other.write_idx);
			};

			constexpr bool operator>=(random_access_it other) const noexcept
			{
				return (data_ptr == other.data_ptr) & (capacity == other.capacity) & (write_idx >= other.write_idx);
			};

			constexpr bool operator<=(random_access_it other) const noexcept
			{
				return (data_ptr == other.data_ptr) & (capacity == other.capacity) & (write_idx <= other.write_idx);
			};

			constexpr bool operator==(random_access_it other) const noexcept
			{
				return (data_ptr == other.data_ptr) & (capacity == other.capacity) & (write_idx == other.write_idx);
			};

			constexpr bool operator!=(random_access_it other) const noexcept
			{
				return !(*this == other);
			};
		};

		using iterator               = random_access_it;
		using const_iterator         = random_access_it;
		using reverse_iterator       = ::std::reverse_iterator<iterator>;
		using const_reverse_iterator = ::std::reverse_iterator<const_iterator>;

		using rebind_allocator_type =
		typename ::std::allocator_traits<allocator_type>::template rebind_alloc<value_type>;

	private:
		pointer                                     data_ptr            = {};
		size_t                                      read_idx            = {};
		size_t                                      write_idx           = {};
		details::compressed_pair<Allocator, size_t> _capacity_allocator = {};

		constexpr void _cleanup() noexcept
		{
			// orphan iterators?
			size_t cap = _capacity_allocator.second();

			if (cap) {
				size_t read_location  = (read_idx % cap);
				size_t write_location = (write_idx % cap);
				if (read_location < write_location) { // read_ptr < write_ptr
					details::destroy(data_ptr + read_location, data_ptr + write_location);
				} else {
					details::destroy(data_ptr, data_ptr + write_location);
					details::destroy(data_ptr + read_location, data_ptr + cap);
				}
			}

			get_allocator().deallocate(data_ptr, cap);
			data_ptr  = nullptr;
			read_idx  = 0;
			write_idx = 0;

			_capacity_allocator.second() = 0ULL;
		}

		constexpr void reserve_with_uninitialized_gap_for_insert(size_type trailing, size_t insert_idx, size_t insert_count)
		{
			auto   old_capacity_and_size = capacity_and_size();
			size_t new_capacity          = trailing + insert_count;

			if (old_capacity_and_size.capacity < new_capacity) {
				if (new_capacity > max_size()) {
					throw std::length_error("cannot allocate larger than max_size");
				}

				size_t read_location =
				old_capacity_and_size.capacity ? (read_idx % old_capacity_and_size.capacity) : read_idx;
				size_t write_location = old_capacity_and_size.capacity ? (write_idx % old_capacity_and_size.capacity)
				: write_idx;
				size_t insert_location = old_capacity_and_size.capacity ? (insert_idx % old_capacity_and_size.capacity)
				: insert_idx;

				size_t copied_size    = read_location < write_location
				? (write_location - read_location)
				: write_location + (old_capacity_and_size.capacity - read_location);

				// we need a bit of extra space for saftey
				const pointer newdata = get_allocator().allocate(new_capacity);
				try {
					// copy data over, we'll place the read_pointer at data_pointer since we have to rearrange the data
					// anyway
					if (read_location < insert_location) { // read_ptr < write_ptr
						::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
												  std::make_move_iterator(data_ptr + insert_location), newdata);
					} else {
						::std::uninitialized_copy(std::make_move_iterator(data_ptr),
												  std::make_move_iterator(data_ptr + insert_location), newdata);
						::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
												  std::make_move_iterator(data_ptr + old_capacity_and_size.capacity),
												  newdata + insert_location);
					}
				} catch (...) {
					get_allocator().deallocate(newdata, new_capacity);
					throw;
				}

				if (data_ptr) {
					// already moved, delete
					get_allocator().deallocate(data_ptr, old_capacity_and_size.capacity);
				}

				data_ptr  = newdata;
				read_idx  = 0;
				write_idx = copied_size + insert_count;
				// write_idx = old_capacity_and_size.size;

				_capacity_allocator.second() = new_capacity;
			}
		}

	public:
		struct capacity_and_size_t {
			size_t capacity;
			size_t size;

			constexpr bool empty() const noexcept
			{
				return size == 0;
			}

			constexpr bool full() const noexcept
			{
				return size >= capacity;
			}
		};

		constexpr capacity_and_size_t capacity_and_size() const noexcept
		{
			capacity_and_size_t cap_size;
			cap_size.capacity = _capacity_allocator.second();
			cap_size.size     = 0;
			if (cap_size.capacity == 0)
			return cap_size;

			cap_size.size = write_idx - read_idx;
			if (read_idx <= write_idx)
			return cap_size;
			else
			return cap_size.size = (cap_size.capacity - cap_size.size), cap_size;
		}

		constexpr ~circular_buffer() noexcept
		{
			_cleanup();
		}

		[[nodiscard]] constexpr allocator_type get_allocator() const noexcept
		{
			return static_cast<allocator_type>(_capacity_allocator.first());
		}

		constexpr circular_buffer() noexcept(::std::is_nothrow_default_constructible_v<rebind_allocator_type>)
		: _capacity_allocator(details::zero_then_variadic_args_t{})
		{
		}

		constexpr explicit circular_buffer(const Allocator& alloc) noexcept
		: _capacity_allocator(details::one_then_variadic_args_t{}, alloc)
		{
		}

		constexpr circular_buffer(size_type count, const value_type& value)
		: _capacity_allocator(details::zero_then_variadic_args_t{})
		{
			if (count) {
				cleared_reserve(count);
				::std::uninitialized_fill(data_ptr, data_ptr + count, value);
				write_idx = count;
			}
		}

		constexpr explicit circular_buffer(size_type count, const value_type& value, const allocator_type& alloc)
		: _capacity_allocator(details::one_then_variadic_args_t{}, alloc)
		{
			if (count) {
				cleared_reserve(count);
				::std::uninitialized_fill(data_ptr, data_ptr + count, value);
				write_idx = count;
			}
		}

		template<class Iterator>
		constexpr circular_buffer(Iterator first, Iterator last)
		: _capacity_allocator(details::zero_then_variadic_args_t{})
		{
			size_type count = std::distance(first, last);
			if (count) {
				cleared_reserve(count);
				::std::uninitialized_copy(first, last, data_ptr);
				write_idx = count;
			}
		}

		#if defined __has_include
		#if __has_include(<ranges>)
		template<std::ranges::range R>
		constexpr circular_buffer(const R range_object) : _capacity_allocator(details::zero_then_variadic_args_t{})
		{
			size_type count = std::ranges::distance(range_object);
			if (count) {
				cleared_reserve(count);
				::std::uninitialized_copy(range_object.begin(), range_object.end(), data_ptr);
				write_idx = count;
			}
		}
		#endif
		#endif

		constexpr circular_buffer(const circular_buffer& other)
		: _capacity_allocator(details::one_then_variadic_args_t{},
		  ::std::allocator_traits<allocator_type>::select_on_container_copy_construction(
			  other._capacity_allocator.first()))
		{

			auto other_capacity_and_size = capacity_and_size();
			if (other_capacity_and_size.size) {
				cleared_reserve(other_capacity_and_size.size);

				size_t read_location  = other_capacity_and_size.capacity
				? (other.read_idx % other_capacity_and_size.capacity)
				: other.read_idx;
				size_t write_location = other_capacity_and_size.capacity
				? (other.write_idx % other_capacity_and_size.capacity)
				: other.write_idx;

				if (read_location < write_location) { // read_ptr < write_ptr
					::std::uninitialized_copy(
						other.data_ptr + read_location, other.data_ptr + write_location, data_ptr);
				} else {
					::std::uninitialized_copy(other.data_ptr, other.data_ptr + write_location, data_ptr);
					::std::uninitialized_copy(other.data_ptr + read_location,
											  other.data_ptr + other_capacity_and_size.capacity, data_ptr + write_location);
				}
			}
			read_idx  = 0;
			write_idx = other_capacity_and_size.size;
		}

		constexpr circular_buffer(const circular_buffer& other, const allocator_type& alloc)
		: _capacity_allocator(details::one_then_variadic_args_t{}, alloc)
		{
			if (!other.empty())
			this->operator=(other);
		}

		constexpr circular_buffer(circular_buffer&& other) : _capacity_allocator(details::zero_then_variadic_args_t{})
		{
			if (!other.empty())
			this->operator=(::std::move(other));
		}

		// assign's
		constexpr void assign(size_type count, const T& value)
		{
			clear();
			if (count > capacity())
			cleared_reserve(count);
			::std::uninitialized_fill(data_ptr, data_ptr + count, value);
			write_idx = count;
		};

		template<typename Iterator> constexpr void assign(Iterator first, Iterator last)
		{
			if constexpr (::std::is_same<::std::random_access_iterator_tag,
				::std::iterator_traits<Iterator>::iterator_category>::value) {
				size_type count = static_cast<size_type>(last - first);
				clear();
				if (count > capacity())
				cleared_reserve(count);
				::std::uninitialized_copy(first, last, data_ptr);
				write_idx = count;
			} else {
				size_type count = std::distance(first, last);
				clear();
				if (count > capacity())
				cleared_reserve(count);

				for (; first != last; ++first) {
					::new ((void*)data_ptr + write_idx) value_type(*first);
					write_idx += 1;
				}
			}
		}
		constexpr void assign(::std::initializer_list<T> ilist)
		{
			assign(ilist.begin(), ilist.end());
		};
		#if defined __has_include
		#if __has_include(<ranges>)
		template<std::ranges::range R> constexpr void assign(const R range_object)
		{
			assign(range_object.begin(), range_object.end());
		}
		#endif
		#endif
		constexpr circular_buffer& operator=(circular_buffer&& other) noexcept(
			::std::is_nothrow_move_assignable<details::compressed_pair<Allocator, size_t>>::value)
		{
			if (this != &other) {
				_cleanup();
				details::pocma(_capacity_allocator.first(), other._capacity_allocator.first());
				read_idx                     = std::move(other.read_idx);
				write_idx                    = std::move(other.write_idx);
				_capacity_allocator.second() = ::std::move(other._capacity_allocator.second());
			}
			return *this;
		}

		constexpr circular_buffer& operator=(const circular_buffer& other)
		{
			if (this != &other) {
				if constexpr (details::should_pocca(_capacity_allocator.first())) {
					if (!::std::allocator_traits<Allocator>::is_always_equal::value &&
						_capacity_allocator.first() != other._capacity_allocator.first()) {
						_cleanup();
					}
					details::pocca(_capacity_allocator.first(), other._capacity_allocator.first());
					assign(other.begin(), other.end());
				} else {
					assign(other.begin(), other.end());
				}
			}
			return *this;
		}

		constexpr circular_buffer& operator=(::std::initializer_list<T> ilist)
		{
			assign(ilist.begin(), ilist.end());
			return *this;
		}

		[[nodiscard]] constexpr T* data() noexcept
		{
			return data_ptr;
		};
		[[nodiscard]] constexpr const T* data() const noexcept
		{
			return data_ptr;
		};

		// begin's
		[[nodiscard]] constexpr iterator begin() noexcept
		{
			return iterator{data_ptr, read_idx, read_idx, _capacity_allocator.second()};
		};
		[[nodiscard]] constexpr const_iterator begin() const noexcept
		{
			return iterator{data_ptr, read_idx, read_idx, _capacity_allocator.second()};
		};
		[[nodiscard]] constexpr const_iterator cbegin() const noexcept
		{
			return iterator{data_ptr, read_idx, read_idx, _capacity_allocator.second()};
		};
		// rbegin's
		[[nodiscard]] constexpr reverse_iterator rbegin() noexcept
		{
			return reverse_iterator(end());
		};
		[[nodiscard]] constexpr const_reverse_iterator rbegin() const noexcept
		{
			return const_reverse_iterator(end());
		};
		[[nodiscard]] constexpr const_reverse_iterator crbegin() const noexcept
		{
			return const_reverse_iterator(end());
		};
		// end's
		[[nodiscard]] constexpr iterator end() noexcept
		{
			return iterator{data_ptr, write_idx, write_idx, _capacity_allocator.second()};
		};
		[[nodiscard]] constexpr const_iterator end() const noexcept
		{
			return iterator{data_ptr, write_idx, write_idx, _capacity_allocator.second()};
		};
		[[nodiscard]] constexpr const_iterator cend() const noexcept
		{
			return iterator{data_ptr, write_idx, write_idx, _capacity_allocator.second()};
		};
		// rend's
		[[nodiscard]] constexpr reverse_iterator rend() noexcept
		{
			return reverse_iterator(begin());
		};
		[[nodiscard]] constexpr const_reverse_iterator rend() const noexcept
		{
			return const_reverse_iterator(begin());
		};
		[[nodiscard]] constexpr const_reverse_iterator crend() const noexcept
		{
			return const_reverse_iterator(begin());
		};

		// size
		constexpr size_type size() const noexcept
		{
			size_t rw  = write_idx - read_idx; // static_cast<size_type>(write_ptr - read_ptr);
			size_t cap = _capacity_allocator.second();
			if (cap == 0)
			return 0;

			if (write_idx >= read_idx) {
				return rw;
			} else {
				return cap - rw;
			}
		};

		// capacity
		constexpr size_type capacity() const noexcept
		{
			return _capacity_allocator.second();
		};

		// max_size (constant)
		constexpr size_type max_size() const noexcept
		{
			constexpr size_type system_max_size    = ((~size_type{0}) / sizeof(T));
			const size_type     allocator_max_size = std::allocator_traits<allocator_type>::max_size(get_allocator());
			const size_type     result = (system_max_size < allocator_max_size) ? system_max_size : allocator_max_size;
			return result;
		};

		// reserve's
		constexpr void reserve(size_type new_capacity)
		{
			auto old_capacity_and_size = capacity_and_size();

			if (old_capacity_and_size.capacity < new_capacity) {
				if (new_capacity > max_size()) {
					throw std::length_error("cannot allocate larger than max_size");
				}

				size_t read_location =
				old_capacity_and_size.capacity ? (read_idx % old_capacity_and_size.capacity) : read_idx;
				size_t write_location = old_capacity_and_size.capacity ? (write_idx % old_capacity_and_size.capacity)
				: write_idx;
				size_t copied_size    = read_location < write_location
				? (write_location - read_location)
				: write_location + (old_capacity_and_size.capacity - read_location);

				// we need a bit of extra space for saftey
				const pointer newdata = get_allocator().allocate(new_capacity + 1);
				try {
					// copy data over, we'll place the read_pointer at data_pointer since we have to rearrange the data
					// anyway
					if (read_location < write_location) { // read_ptr < write_ptr
						::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
												  std::make_move_iterator(data_ptr + write_location), newdata);
					} else {
						// move the back half to the front
						::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
												  std::make_move_iterator(data_ptr + old_capacity_and_size.capacity),
												  newdata);
						// move the front half to the back
						::std::uninitialized_copy(std::make_move_iterator(data_ptr),
												  std::make_move_iterator(data_ptr + write_location),
												  newdata + (old_capacity_and_size.capacity-read_location));
					}
				} catch (...) {
					get_allocator().deallocate(newdata, new_capacity);
					throw;
				}

				if (data_ptr) {
					// already moved, delete
					get_allocator().deallocate(data_ptr, old_capacity_and_size.capacity);
				}

				data_ptr  = newdata;
				read_idx  = 0;
				write_idx = copied_size;
				// write_idx = old_capacity_and_size.size;

				_capacity_allocator.second() = new_capacity;
			}
		}

		constexpr void reserve_front(size_type trailing, size_t leading)
		{
			auto   old_capacity_and_size = capacity_and_size();
			size_t new_capacity          = trailing + leading;

			if (old_capacity_and_size.capacity < new_capacity) {
				if (new_capacity > max_size()) {
					throw std::length_error("cannot allocate larger than max_size");
				}

				size_t read_location =
				old_capacity_and_size.capacity ? (read_idx % old_capacity_and_size.capacity) : read_idx;
				size_t write_location = old_capacity_and_size.capacity ? (write_idx % old_capacity_and_size.capacity)
				: write_idx;
				size_t copied_size    = read_location < write_location
				? (write_location - read_location)
				: write_location + (old_capacity_and_size.capacity - read_location);

				// we need a bit of extra space for saftey
				const pointer newdata = get_allocator().allocate(new_capacity);
				try {
					// copy data over, we'll place the read_pointer at data_pointer since we have to rearrange the data
					// anyway
					if (read_location < write_location) { // read_ptr < write_ptr
						::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
												  std::make_move_iterator(data_ptr + write_location), newdata + leading);
					} else {
						::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
												  std::make_move_iterator(data_ptr + old_capacity_and_size.capacity),
												  newdata + leading);
						::std::uninitialized_copy(std::make_move_iterator(data_ptr),
												  std::make_move_iterator(data_ptr + write_location),
												  newdata + (leading + old_capacity_and_size.capacity - read_location));
					}
				} catch (...) {
					get_allocator().deallocate(newdata, new_capacity);
					throw;
				}

				if (data_ptr) {
					// already moved, delete
					get_allocator().deallocate(data_ptr, old_capacity_and_size.capacity);
				}

				data_ptr  = newdata;
				read_idx  = leading;
				write_idx = copied_size + leading;
				// write_idx = old_capacity_and_size.size;

				_capacity_allocator.second() = new_capacity;
			}
		}

		constexpr void clear() noexcept
		{
			if constexpr (!::std::is_trivially_constructible<value_type>::value) {
				auto old_capacity_and_size = capacity_and_size();

				size_t read_location =
				old_capacity_and_size.capacity ? (read_idx % old_capacity_and_size.capacity) : read_idx;
				size_t write_location = old_capacity_and_size.capacity ? (write_idx % old_capacity_and_size.capacity)
				: write_idx;
				// copy data over, we'll place the read_pointer at data_pointer since we have to rearrange the data
				// anyway
				if (read_location < write_location) { // read_ptr < write_ptr
					details::destroy(data_ptr + read_location, data_ptr + write_location);
				} else {
					details::destroy(data_ptr, data_ptr + write_location);
					details::destroy(data_ptr + read_location, data_ptr + old_capacity_and_size.capacity);
				}
			}
			read_idx  = 0;
			write_idx = 0;
		}

		// clear
		// NOTE: use only after clear();
		constexpr void cleared_reserve(size_type new_capacity)
		{
			const pointer newdata = get_allocator().allocate(new_capacity);
			if (data_ptr) {
				get_allocator().deallocate(data_ptr, _capacity_allocator.second());
			}
			data_ptr  = newdata;
			read_idx  = 0;
			write_idx = 0;

			_capacity_allocator.second() = new_capacity;
		}

		// pop_back's
		constexpr void pop_back()
		{
			auto cs = capacity_and_size();
			if (cs.size) [[likely]] {
				if constexpr (::std::is_trivially_destructible<value_type>::value) {
					write_idx -= (write_idx > read_idx);
				} else {
					write_idx -= (write_idx > read_idx);
					if (cs.size <= cs.capacity)
					destroy_it(data_ptr + (write_idx % _capacity_allocator.second()));
					//(data_ptr+(write_index%_capacity_allocator.second()))->value_type();
				}
			} else {
				// error ?
			}
		};

		// pop_front's
		constexpr void pop_front()
		{
			auto cs = capacity_and_size();
			if (cs.size) {
				if constexpr (::std::is_trivially_destructible<value_type>::value) {
					read_idx += (write_idx > read_idx);
				} else {
					size_t tmp_read_idx = read_idx;
					read_idx += (write_idx > read_idx);
					if (cs.size <= cs.capacity)
					destroy_it(data_ptr + (tmp_read_idx - 1 % _capacity_allocator.second()));
				}
			}
		}

		// make_contiguous
		// linearizes the contents of the circular_buffer, puts circular_buffer in a state where an equivlent
		// std::vector, this would be like constructing a new circular_buffer from the current contents
		constexpr circular_buffer make_contiguous() noexcept
		{
			auto   cs             = capacity_and_size();
			size_t read_location  = cs.capacity ? (read_idx % cs.capacity) : read_idx;
			size_t write_location = cs.capacity ? (write_idx % cs.capacity) : write_idx;
			// if we're not already setup on the [0]th address already
			if (read_location) {
				if constexpr (::std::is_trivially_destructible<value_type>::value) {
					// simplest case, we can act as if we're full, so we just rotate around to bring read_location to
					// the front
					::std::rotate(data_ptr, data_ptr + read_location, data_ptr + cs.capacity);
				} else {
					if (cs.size >= cs.capacity) {
						// simplest case, we're full, so we just rotate around to bring read_location to the front
						::std::rotate(data_ptr, data_ptr + read_location, data_ptr + cs.capacity);
					} else {
						if (read_location < write_location) {
							::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
													  ::std::make_move_iterator(data_ptr + write_location), data_ptr);
						} else {
							::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
													  ::std::make_move_iterator(data_ptr + cs.capacity),
													  data_ptr + write_location);
							// if the wrapped location is 0, then we're in an edge case where the
							// data was already contiguous, it was just shifted over
							if (write_location)
							::std::rotate(data_ptr, data_ptr + read_location, data_ptr + cs.size);
						}
					}
				}

				read_idx  = 0;
				write_idx = cs.size;
			}
			return *this;
		}

		constexpr circular_buffer make_contiguous() const noexcept
		{
			auto   cs             = capacity_and_size();
			size_t read_location  = cs.capacity ? (read_idx % cs.capacity) : read_idx;
			size_t write_location = cs.capacity ? (write_idx % cs.capacity) : write_idx;
			// if we're not already setup on the [0]th address already
			if (read_location) {
				if constexpr (::std::is_trivially_destructible<value_type>::value) {
					// simplest case, we can act as if we're full, so we just rotate around to bring read_location to
					// the front
					::std::rotate(data_ptr, data_ptr + read_location, data_ptr + cs.capacity);
				} else {
					if (cs.size >= cs.capacity) {
						// simplest case, we're full, so we just rotate around to bring read_location to the front
						::std::rotate(data_ptr, data_ptr + read_location, data_ptr + cs.capacity);
					} else {
						if (read_location < write_location) {
							::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
													  ::std::make_move_iterator(data_ptr + write_location), data_ptr);
						} else {
							::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
													  ::std::make_move_iterator(data_ptr + cs.capacity),
													  data_ptr + write_location);
							// if the wrapped location is 0, then we're in an edge case where the
							// data was already contiguous, it was just shifted over
							if (write_location)
							::std::rotate(data_ptr, data_ptr + read_location, data_ptr + cs.size);
						}
					}
				}

				read_idx  = 0;
				write_idx = cs.size;
			}
			return *this;
		}

		//[]'s
		[[nodiscard]] constexpr reference operator[](size_type pos)
		{
			assert(pos < size());
			return data_ptr[(read_idx + pos) % capacity()];
		};
		[[nodiscard]] constexpr const_reference operator[](size_type pos) const
		{
			assert(pos < size());
			return data_ptr[(read_idx + pos) % capacity()];
		};

		// emplace_back's
		// Like std::vector<> will reallocate when full
		template<class... Args> constexpr reference emplace_back(Args&&... args)
		{
			auto cs = capacity_and_size();
			if (cs.full()) {
				size_t target_capacity = geometric_int_expansion_policy<2>{}.grow_capacity(
					cs.size, cs.capacity, cs.capacity + 1);
				reserve(target_capacity);
			}

			pointer it = data_ptr + (write_idx % capacity());
			write_idx += 1;
			::new ((void*)it) value_type(::std::forward<Args>(args)...);
			return *it;
		};

		// emplace_back_with_policy
		// Like std::vector<> will reallocate when full
		template<typename... Args, typename ExpansionPolicy>
		constexpr reference emplace_back_with_policy(Args&&... args, ExpansionPolicy)
		{
			auto cs = capacity_and_size();
			if (cs.full()) {
				size_t target_capacity = ExpansionPolicy{}.grow_capacity(cs.size, cs.capacity, cs.capacity + 1);
				reserve(target_capacity);
			}
			pointer it = data_ptr + (write_idx % capacity());
			write_idx += 1;
			::new ((void*)it) value_type(::std::forward<Args>(args)...);
			return *it;
		}

		// emplace_front's
		// Like std::vector<> will reallocate when full
		template<class... Args> constexpr reference emplace_front(Args&&... args)
		{
			auto cs = capacity_and_size();
			if (cs.full()) {
				size_t target_capacity = geometric_int_expansion_policy<2>{}.grow_capacity(
					cs.size, cs.capacity, cs.capacity + 1);
				reserve_front(target_capacity - 1, 1);
			}
			size_t  new_capacity = capacity();
			pointer it           = data_ptr + ((read_idx + new_capacity - 1) % new_capacity);
			// we assume the contents at the write_index are uninitialized
			::new ((void*)it) value_type(::std::forward<Args>(args)...);
			bool rotate_forward = read_idx;
			read_idx += rotate_forward ? -1 : new_capacity - 1;
			write_idx += rotate_forward ? 0 : new_capacity;
			return *it;
		};

		// emplace_front_with_policy
		// Like std::deque<> will reallocate when full
		template<typename... Args, typename ExpansionPolicy>
		constexpr reference emplace_front_with_policy(Args&&... args, ExpansionPolicy)
		{
			auto cs = capacity_and_size();
			if (cs.full()) {
				size_t target_capacity = ExpansionPolicy{}.grow_capacity(cs.size, cs.capacity, cs.capacity + 1);
				reserve_front(target_capacity - 1, 1);
				/*
				size_type     new_capacity = ExpansionPolicy<2>{}.grow_capacity(cs.size, cs.capacity, cs.capacity + 1);
				const pointer newdata      = _capacity_allocator.first().allocate(new_capacity);

				size_t read_location  = cs.capacity ? (read_idx % cs.capacity) : read_idx;
				size_t write_location = cs.capacity ? (write_idx % cs.capacity) : write_idx;

				size_t copied_size       = read_location < write_location
				? (write_location - read_location)
				: write_location + (old_capacity_and_size.capacity-read_location);
				try {
				::std::allocator_traits<allocator_type>::construct(
				_capacity_allocator.first(), newdata, std::forward<Args>(args)...);

				if (read_location < write_location) { // read_ptr < write_ptr
				::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
				std::make_move_iterator(data_ptr + write_location), newdata + 1);
				} else {
				::std::uninitialized_copy(std::make_move_iterator(data_ptr),
				std::make_move_iterator(data_ptr + write_location), newdata);
				::std::uninitialized_copy(std::make_move_iterator(data_ptr + read_location),
				std::make_move_iterator(data_ptr + old_capacity_and_size.capacity),
				newdata + write_location + 1);
				}
				} catch (...) {
				_capacity_allocator.first().deallocate(newdata, new_capacity);
				throw;
				}

				if (data_ptr) {
				_capacity_allocator.first().deallocate(data_ptr, cs.capacity);
				}

				data_ptr                     = newdata;
				read_idx                     = 0;
				write_idx                    = coped_size + 1;
				_capacity_allocator.second() = new_capacity;
				return;
				*/
			}

			size_t  new_capacity = capacity();
			pointer it           = data_ptr + ((read_idx + new_capacity - 1) % new_capacity);
			// we assume the contents at the write_index are uninitialized
			::new ((void*)it) value_type(::std::forward<Args>(args)...);
			bool rotate_forward = read_idx;
			read_idx += rotate_forward ? -1 : new_capacity - 1;
			write_idx += rotate_forward ? 0 : new_capacity;
			return *it;
		}

		template<class InputIt, class EndSentinel>
		constexpr iterator insert_range(const_iterator pos, InputIt first, EndSentinel last)
		{
			if (first == last)
			return;

			assert(pos >= cbegin() && pos <= cend() && "insert iterator is out of bounds");
			size_t insert_idx = ::std::distance(cbegin(), pos);
			auto   cs         = capacity_and_size();

			if constexpr (::std::is_same<::std::random_access_iterator_tag,
				::std::iterator_traits<InputIt>::iterator_category>::value) {
				size_type insert_count = last - first;

				if (cs.size + insert_count >= cs.capacity) {
					size_t target_capacity = geometric_int_expansion_policy<2>{}
					.grow_capacity(cs.size, cs.capacity, cs.capacity + insert_count);
					reserve(target_capacity);
				}

				::std::uninitialized_copy(first, last, end());
				write_idx += insert_count;
			} else {
				auto old_size = cs.size;
				for (; first != last && old_size < cs.capacity; ++first, ++old_size) {
					uninitialized_overwrite_index(*first);
				}
				for (; first != last; ++first) {
					emplace_back(*first);
				}
			}

			::std::rotate(begin() + insert_idx, begin() + cs.size, end());
			return begin() + (insert_idx%cs.capacity);
		}

		// insert's
		constexpr iterator insert(const_iterator pos, const T& value)
		{
			return emplace(pos, value);
		}
		constexpr iterator insert(const_iterator pos, T&& value)
		{
			return emplace(pos, ::std::move(value));
		}
		template<class InputIt> constexpr iterator insert(const_iterator pos, InputIt first, InputIt last)
		{
			return insert_range(pos, first, last);
		};
		constexpr iterator insert(const_iterator pos, ::std::initializer_list<T> ilist)
		{
			return insert(pos, ilist.begin(), ilist.end());
		};

		// emplace's
		template<class... Args> constexpr iterator emplace(const_iterator pos, Args&&... args)
		{
			auto      cs         = capacity_and_size();
			size_type insert_idx = pos.write_idx - cbegin().write_idx;

			const_iterator begin_it = cbegin();
			const_iterator end_it   = cend();
			assert(pos >= begin_it && pos <= end_it && "emplace iterator does not refer to this vector");

			if (cs.size < cs.capacity) {
				if (pos == end_it) {
					uninitialized_overwrite_write_index(::std::forward<Args>(args)...);
				} else if (pos == begin_it) {
					uninitialized_overwrite_read_index(::std::forward<Args>(args)...);
				} else {
					iterator start  = end();
					iterator last   = start;
					iterator target = start + 1;
					// uninitialized_move_backward
					for (; target != last; target--, start--) {
						::std::allocator_traits<allocator_type>::construct(
							_capacity_allocator.first(), ::std::to_address(target), ::std::move(*start));
					}
					::std::allocator_traits<allocator_type>::construct(
						_capacity_allocator.first(), ::std::to_address(pos), std::forward<Args>(args)...);
					write_idx += 1;
				}
			} else {
				if (pos == end_it) {
					emplace_back(::std::forward<Args>(args)...);
				} else if (pos == begin_it) {
					emplace_front();
				} else {
					size_type new_capacity = geometric_int_expansion_policy<2>{}.grow_capacity(
						cs.size, cs.capacity, cs.capacity + 1);
					const pointer newdata = _capacity_allocator.first().allocate(new_capacity);

					try {
						::std::allocator_traits<allocator_type>::construct(
							_capacity_allocator.first(), newdata + insert_idx, std::forward<Args>(args)...);

						::std::uninitialized_copy(::std::make_move_iterator(begin()),
												  ::std::make_move_iterator(begin() + insert_idx), newdata);
						if (cs.size > cs.capacity) {
							::std::uninitialized_copy(::std::make_move_iterator(begin() + insert_idx),
													  ::std::make_move_iterator(begin() + cs.capacity), newdata + insert_idx + 1);
						} else {
							::std::uninitialized_copy(::std::make_move_iterator(begin() + insert_idx),
													  ::std::make_move_iterator(end()), newdata + insert_idx + 1);
						}
					} catch (...) {
						_capacity_allocator.first().deallocate(newdata, new_capacity);
						throw;
					}

					if (data_ptr) {
						_capacity_allocator.first().deallocate(data_ptr, cs.capacity);
					}

					data_ptr                     = newdata;
					read_idx                     = 0;
					write_idx                    = cs.capacity + 1;
					_capacity_allocator.second() = new_capacity;
				}
			}
			return begin() + insert_idx;
		}

		#if defined(__has_include)
		#if __has_include(<functional>)
		// Assumes we take a pointer and a size argument callback
		template<class Operation, ::std::enable_if<::std::is_convertible_v<Operation,
		::std::function<size_t(value_type*, size_t)>>,
		bool>::type = true>
		constexpr void resize_and_overwrite(size_t count, Operation op)
		{
			auto cs = capacity_and_size();

			if (count <= cs.size && (cs.size < cs.capacity)) {
				// destroy the trailing elements
				make_contiguous();
				::std::destroy_n(data_ptr + count, cs.capacity - cs.size);
			} else if (count > cs.capacity) {
				//
				size_t target_capacity = geometric_int_expansion_policy<2>{}.grow_capacity(cs.size, cs.capacity, count);
				const pointer newdata  = get_allocator().allocate(target_capacity + 1);

				size_t read_location  = cs.capacity ? (read_idx % cs.capacity) : read_idx;
				size_t write_location = cs.capacity ? (write_idx % cs.capacity) : write_idx;
				try {
					if (read_location < write_location) { // read_ptr < write_ptr
						::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
												  ::std::make_move_iterator(data_ptr + write_location), newdata);
					} else {
						::std::uninitialized_copy(::std::make_move_iterator(data_ptr),
												  ::std::make_move_iterator(data_ptr + write_location), newdata);
						::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
												  ::std::make_move_iterator(data_ptr + cs.capacity), newdata + write_location);
					}
				} catch (...) {
					get_allocator().deallocate(newdata, target_capacity);
					throw;
				}

				if (data_ptr) {
					// already moved, delete
					get_allocator().deallocate(data_ptr, cs.capacity);
				}

				data_ptr = newdata;

				size_t new_size = op(newdata, cs.size < cs.capacity ? cs.size : cs.capacity);
				read_idx        = 0;
				write_idx       = new_size;
				return;
			} else {
				make_contiguous();
			}

			size_t new_size = op(data_ptr, cs.size < cs.capacity ? cs.size : cs.capacity);
			read_idx        = 0;
			write_idx       = new_size;
		}

		struct read_idx_t {
			size_t read_idx;
		};

		struct write_idx_t {
			size_t write_idx;
		};

		// resize_and_overwrite
		// overwrites given a provided callback, expects the overwrite operation
		template<class Operation,
		::std::enable_if<::std::is_convertible_v<Operation, ::std::function<size_t(value_type*, size_t,
			read_idx_t, write_idx_t)>>,
		bool>::type = true>
		constexpr void resize_and_overwrite(size_t count, Operation op)
		{
			auto cs = capacity_and_size();

			size_t mn_cs = cs.size < cs.capacity ? cs.size : cs.capacity;

			if (count <= cs.size && (cs.size < cs.capacity)) {
				// destroy the trailing elements
				make_contiguous();
				::std::destroy_n(data_ptr + count, cs.capacity - cs.size);
			} else if (count > cs.capacity) {
				//
				size_t target_capacity = geometric_int_expansion_policy<2>{}.grow_capacity(cs.size, cs.capacity, count);
				const pointer newdata  = get_allocator().allocate(target_capacity + 1);

				size_t read_location  = cs.capacity ? (read_idx % cs.capacity) : read_idx;
				size_t write_location = cs.capacity ? (write_idx % cs.capacity) : write_idx;
				try {
					if (read_location < write_location) { // read_ptr < write_ptr
						::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
												  ::std::make_move_iterator(data_ptr + write_location), newdata);
					} else {
						::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
												  ::std::make_move_iterator(data_ptr + cs.capacity), newdata);

						::std::uninitialized_copy(::std::make_move_iterator(data_ptr),
												  ::std::make_move_iterator(data_ptr + write_location), newdata + (cs.capacity-read_location));
					}
				} catch (...) {
					get_allocator().deallocate(newdata, target_capacity);
					throw;
				}

				if (data_ptr) {
					// already moved, delete
					get_allocator().deallocate(data_ptr, cs.capacity);
				}

				data_ptr = newdata;

				size_t new_size = op(newdata, mn_cs, read_idx_t{0}, write_idx_t{cs.size});
				read_idx        = 0;
				write_idx       = new_size;
				return;
			}

			size_t new_size = op(data_ptr, mn_cs, read_idx_t{read_idx}, write_idx_t{write_idx});
			read_idx        = 0;
			write_idx       = new_size;
		}
		#endif
		#endif

		// push_back's
		constexpr void push_back(const T& value)
		{
			emplace_back(::std::forward<const T&>(value));
		}
		constexpr void push_back(T&& value)
		{
			emplace_back(::std::forward<T&&>(value));
		};

		// push_fronts's
		constexpr void push_front(const T& value)
		{
			emplace_front(::std::forward<const T&>(value));
		}
		constexpr void push_front(T&& value)
		{
			emplace_front(::std::forward<T&&>(value));
		};

		// overwrite_back
		// overwrites the contents of back(), then bumps the write index forward and read index if needed
		// doesn't allocate
		template<class... Args> constexpr reference overwrite_back(Args&&... args)
		{
			auto    cs      = capacity_and_size();
			bool    is_full = cs.full();
			pointer it      = data_ptr + (write_idx % cs.capacity);

			if constexpr (::std::is_trivially_destructible<T>::value) {
				::new ((void*)it) value_type(::std::forward<Args>(args)...);
			} else {
				// if moving / copying is faster this is better
				if constexpr (::std::is_trivially_copyable<T>::value ||
					::std::is_trivially_move_constructible<T>::value) {
					*it = value_type{::std::forward<Args>(args)...};
				} else {
					// otherwise we're going the destroy / construct route
					if (is_full)
					::std::destroy_at(it);
					::new ((void*)it) value_type(::std::forward<Args>(args)...);
				}
			}

			write_idx += 1;
			read_idx += is_full; // if we're full, maintain the calculated size*

			return *it;
		};

		// overwrite_back
		// overwrites the contents of the current write index, then bumps the write index forward
		// doesn't allocate
		template<class... Args> constexpr reference overwrite_write_index(Args&&... args)
		{
			auto    cs      = capacity_and_size();
			bool    is_full = cs.full();
			pointer it      = data_ptr + (write_idx % cs.capacity);
			// read_idx += is_full; // if we're full, maintain the calculated size*
			if constexpr (::std::is_trivially_destructible<T>::value) {
				::new ((void*)it) value_type(::std::forward<Args>(args)...);
			} else {
				// if moving / copying is faster this is better
				if constexpr (::std::is_trivially_copyable<T>::value ||
					::std::is_trivially_move_constructible<T>::value) {
					*it = value_type{::std::forward<Args>(args)...};
				} else {
					// otherwise we're going the destroy / construct route
					if (is_full)
					::std::destroy_at(it);
					::new ((void*)it) value_type(::std::forward<Args>(args)...);
				}
			}
			write_idx += 1;

			return *it;
		};

		// uninitialized_overwrite_write_index
		// overwrites the contents of the current write index (assumes the location is uninitialized), then bumps the
		// write index forward doesn't allocate
		template<class... Args> constexpr reference uninitialized_overwrite_write_index(Args&&... args)
		{
			auto    cs = capacity_and_size();
			pointer it = data_ptr + (write_idx % cs.capacity);
			// we assume the contents at the write_index are uninitialized
			::new ((void*)it) value_type(::std::forward<Args>(args)...);
			write_idx += 1;
			return *it;
		};

		// uninitialized_overwrite_write_index
		// overwrites the contents of the current write index (assumes the location is uninitialized), then moves the
		// read and write indexs around
		template<class... Args> constexpr reference uninitialized_overwrite_read_index(Args&&... args)
		{
			auto    cs = capacity_and_size();
			pointer it = data_ptr + ((read_idx + cs.capacity - 1) % cs.capacity);
			// we assume the contents at the write_index are uninitialized
			::new ((void*)it) value_type(::std::forward<Args>(args)...);
			bool rotate_forward = read_idx;
			read_idx += rotate_forward ? -1 : cs.capacity - 1;
			write_idx += rotate_forward ? 0 : cs.capacity;
			return *it;
		};

		// uninitialized_overwrite_write_index
		// overwrites the contents of the current write index (assumes the location is uninitialized), then bumps the
		// write index forward doesn't allocate
		template<class... Args> constexpr reference initialized_overwrite_write_index(Args&&... args)
		{
			auto    cs      = capacity_and_size();
			bool    is_full = cs.full();
			pointer it      = data_ptr + (write_idx % cs.capacity);
			// read_idx += is_full; // if we're full, maintain the calculated size*
			if constexpr (::std::is_trivially_destructible<T>::value) {
				::new ((void*)it) value_type(::std::forward<Args>(args)...);
			} else {
				// if moving / copying is faster this is better
				if constexpr (::std::is_trivially_copyable<T>::value ||
					::std::is_trivially_move_constructible<T>::value) {
					*it = value_type{::std::forward<Args>(args)...};
				} else {
					// otherwise we're going the destroy / construct route, we don't check if we're full here
					::std::destroy_at(it);
					::new ((void*)it) value_type(::std::forward<Args>(args)...);
				}
			}
			write_idx += 1;

			return *it;
		};

		// empty's
		[[nodiscard]] constexpr bool empty() const noexcept
		{
			return size() == 0;
		};
		// full (non standard)
		[[nodiscard]] constexpr bool full() const noexcept
		{
			auto cap_sz = capacity_and_size();
			return cap_sz.size >= cap_sz.capacity; // size() >= capacity();
		};

		// front
		[[nodiscard]] constexpr reference front()
		{
			assert(!empty());
			return data_ptr[read_idx % capacity()]; // read_ptr[0];
		};
		[[nodiscard]] constexpr const_reference front() const
		{
			assert(!empty());
			return data_ptr[read_idx % capacity()]; // read_ptr[0];
		};

		// back's
		[[nodiscard]] constexpr reference back()
		{
			assert(!empty());
			return data_ptr[(write_idx - 1) % capacity()];
		};
		[[nodiscard]] constexpr const_reference back() const
		{
			assert(!empty());
			return data_ptr[(write_idx - 1) % capacity()];
		};

		// unchecked_reserve
		// ignores capacity checks and allocates anyway
		// note: think shrink_to_fit
		constexpr void unchecked_reserve(size_type new_capacity)
		{
			auto old_capacity_and_size = capacity_and_size();

			size_t read_location =
			old_capacity_and_size.capacity ? (read_idx % old_capacity_and_size.capacity) : read_idx;
			size_t write_location =
			old_capacity_and_size.capacity ? (write_idx % old_capacity_and_size.capacity) : write_idx;

			// we need a bit of extra space for saftey
			const pointer newdata = get_allocator().allocate(new_capacity + 1);
			try {
				// copy data over, we'll place the read_pointer at data_pointer since we have to rearrange the data
				// anyway
				if (read_location < write_location) { // read_ptr < write_ptr
					::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
											  ::std::make_move_iterator(data_ptr + write_location), newdata);
				} else {
					::std::uninitialized_copy(::std::make_move_iterator(data_ptr + read_location),
											  ::std::make_move_iterator(data_ptr + old_capacity_and_size.capacity),
											  newdata);

					::std::uninitialized_copy(::std::make_move_iterator(data_ptr),
											  ::std::make_move_iterator(data_ptr + write_location), newdata + (old_capacity_and_size.capacity-read_location));
				}
			} catch (...) {
				get_allocator().deallocate(newdata, new_capacity);
				throw;
			}

			if (data_ptr) {
				// already moved, delete
				get_allocator().deallocate(data_ptr, old_capacity_and_size.capacity);
			}

			data_ptr  = newdata;
			read_idx  = 0;
			write_idx = old_capacity_and_size.size;

			_capacity_allocator.second() = new_capacity;
		}

		constexpr void shrink_to_fit()
		{
			auto cs = capacity_and_size();
			if (cs.size < cs.capacity) {
				if (cs.size == 0) { //_begin == _end
					_cleanup();     // if we're truly empty, remove the allocation...
				} else {
					unchecked_reserve(cs.size());
				}
			}
		}
	};

} // namespace circular_buffer

namespace std::pmr {
	template<class T>
	using circular_buffer = ::circular_buffer::circular_buffer<T, ::std::pmr::polymorphic_allocator<T>>;
}