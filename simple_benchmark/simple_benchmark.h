#pragma once

#include <vector>
#include <memory_resource>
#include <string>
#include <string_view>
#include <chrono>

//A shameless rip of the bits that appear to make nanobench work

namespace simple_bench {
	using Clock = std::conditional<std::chrono::high_resolution_clock::is_steady, std::chrono::high_resolution_clock,
	                               std::chrono::steady_clock>::type;

	template <size_t N> struct timing_result {
		size_t nanoseconds[N];
	};


	template <size_t N> struct batch_result {
		std::string title;
		timing_result<N> result;
		size_t           loops;
		size_t           batch_count;

		constexpr double get_average_operation_ns(size_t index) noexcept {
			return (double)result.nanoseconds[index] / (double)(loops * batch_count);
		}
		constexpr double get_total_ns(size_t index) noexcept {
			return (double)result.nanoseconds[index];
		}
	};
	// same as above but with a string_view if we don't need to allocate
	template <size_t N> struct c_batch_result {
		std::string_view title;
		timing_result<N> result;
		size_t           loops;
		size_t           batch_count;

		constexpr double get_average_operation_ns(size_t index) noexcept {
			return (double)result.nanoseconds[index] / (double)(loops * batch_count);
		}
		constexpr double get_total_ns(size_t index) noexcept {
			return (double)result.nanoseconds[index];
		}
	};

	[[nodiscard]] Clock::duration get_clock_resolution(size_t iterations = 20) noexcept {
		size_t            time_diff = ~size_t{0};
		Clock::time_point ts_0;
		Clock::time_point ts_1;
		for (size_t i = 0; i < iterations; ++i) {
			ts_0 = Clock::now();
			do {
				ts_1 = Clock::now();
			} while (ts_0 == ts_1);
			size_t ns_diff = (ts_1 - ts_0).count();
			time_diff      = (time_diff < ns_diff) ? time_diff : ns_diff;
		}
		return std::chrono::nanoseconds{time_diff};
	}

	[[nodiscard]] Clock::duration get_target_time(size_t iterations = 20) noexcept {
		auto resolution = get_clock_resolution(iterations);
		return resolution * 1000;
	}

	[[nodiscard]] uint64_t get_iterations(std::chrono::nanoseconds elapsed, uint64_t iters) noexcept {
		double d_elapsed     = (double)elapsed.count();
		auto   d_target_time = get_target_time();
		auto   d_new_iters   = (double)d_target_time.count() / d_elapsed * (double)iters;

		d_new_iters *= 1.0 + 0.2;

		return static_cast<uint64_t>(d_new_iters + 0.5);
	}
  
  /*
  Note: I purposefully discard a whole result in order to "warmup" and get the rough loop count needed.
  For longer running benchmarks it'd probably be better to write this but keep the first run.
  Nanobench takes several functions running back to back as a single sample and runs that in a loop, with a switch statement running a small state machine.
  I'm dumbing this down here, effectively this function takes "one" nanobench sample, with one sample as a warmup. They also keep all the samples to display
  statistics (error rate, median etc...). The functions I'm testing run absurdly fast, and the error rates are low, so this small header's a quick rewrite for what I needed.
  
  Slight modification on nanobench that I wanted was to be able to test functions which had setup / cleanup work timed seperate from the actual function. (Pretty easy to do)
  
  However in doing that I'm making an assumption that the setup and cleanup phases will behave similarly between the two test loops, subtracting the one test from the other theoretically only removes the minimum time the setup or cleanup could take, variance isn't accounted for. In fact the variance in the result is compounded. I'm making the assumption the variance is very small.
  */

	/*
	Runs a benchmark which requires a bit of setup and cleanup to work.
	*Assumes setup and cleanup functions can be run back to back without the tested function
	Output:
		result.result.nanoseconds[0] (rough) total time spent in setup and cleanup work
		result.result.nanoseconds[1] (rough) total time spent in actual work
	*/
	template <typename Setup, typename Op, typename Cleanup>
	batch_result<2> run_with_setup(std::string_view v, size_t batch, Setup S, Op O, Cleanup C) {
		batch_result<2> result;
		result.title = v;

		batch = batch ? batch : 1;
		result.batch_count = batch;

		timing_result<2> tmp_0;

		tmp_0.nanoseconds[0] = 0;
		tmp_0.nanoseconds[1] = 0;
		size_t loops_0       = 0;
		size_t loops_1       = 0;

		uint64_t loop_count = 0;
		{
			S();
			auto t0 = Clock::now();
			O();
			auto t1 = Clock::now();
			C();

			loop_count = get_iterations((t1 - t0), 1);
		}
		// Our dirty trick here is we test everything together
		auto t0 = Clock::now();
		do {
			S();
			O();
			C();
			loops_0++;
		} while (loops_0 < loop_count);
		auto t1 = Clock::now();
		tmp_0.nanoseconds[1] = (t1 - t0).count();
		// then test the setup followed by cleanup functions (think malloc / free, constructor / destructor pairs)
		auto t2 = Clock::now();
		do {
			S();
			C();
			loops_1++;
		} while (loops_1 < loop_count);
		// The real dirty trick is we run these two for the same number of loops, which means we don't have to
		// stick timing functions between, we can approximate the real time spent
		auto t3 = Clock::now();
		tmp_0.nanoseconds[0] = (t3 - t2).count();

		result.result.nanoseconds[0] = tmp_0.nanoseconds[0];
		// the dirty trick of keeping the number of loops the same means we can take the total time spent and
		// subtract the total time spent in just the setup and cleanup sections, no adjustments required
		
		result.result.nanoseconds[1] = tmp_0.nanoseconds[1] - tmp_0.nanoseconds[0];

		result.loops = (loop_count + 1);

		return result;
	}

	/*
	Runs a basic benchmark of a function
	Output:
		result.result.nanoseconds[0] (rough) total time spent in actual work
	*/
	template <typename Op> batch_result<1> run(std::string_view v, size_t batch, Op O) {
		batch_result<1> result;
		result.title = v;

		batch              = batch ? batch : 1;
		result.batch_count = batch;
		size_t loop_0      = 0;

		size_t loop_count;
		{
			auto t0 = Clock::now();
			O();
			auto t1 = Clock::now();
			loop_count = get_iterations((t1 - t0), 1);
		}

		auto t0 = Clock::now();
		do {
			O();
			loop_0++;
		} while (loop_0 < loop_count);
		auto t1              = Clock::now();
		result.result.nanoseconds[0] = (t1 - t0).count();
		result.loops                 = loop_count + 1;

		return result;
	}


	template <typename Setup, typename Op, typename Cleanup>
	c_batch_result<2> c_run_with_setup(std::string_view v, size_t batch, Setup S, Op O, Cleanup C) {
		c_batch_result<2> result;
		result.title = v;

		batch              = batch ? batch : 1;
		result.batch_count = batch;

		timing_result<2> tmp_0;

		tmp_0.nanoseconds[0] = 0;
		tmp_0.nanoseconds[1] = 0;
		size_t loops_0       = 0;
		size_t loops_1       = 0;

		uint64_t loop_count = 0;
		{
			S();
			auto t0 = Clock::now();
			O();
			auto t1 = Clock::now();
			C();

			loop_count = get_iterations((t1 - t0), 1);
		}

		auto t0 = Clock::now();
		do {
			S();
			O();
			C();
			loops_0++;
		} while (loops_0 < loop_count);
		auto t1              = Clock::now();
		tmp_0.nanoseconds[1] = (t1 - t0).count();

		auto t2 = Clock::now();
		do {
			S();
			C();
			loops_1++;
		} while (loops_1 < loop_count);
		auto t3              = Clock::now();
		tmp_0.nanoseconds[0] = (t3 - t2).count();

		result.result.nanoseconds[0] = tmp_0.nanoseconds[0];
		result.result.nanoseconds[1] = tmp_0.nanoseconds[1] - tmp_0.nanoseconds[0];

		result.loops = (loop_count + 1);

		return result;
	}

	template <typename Op> c_batch_result<1> c_run(std::string_view v, size_t batch, Op O) {
		c_batch_result<1> result;
		result.title = v;

		batch              = batch ? batch : 1;
		result.batch_count = batch;
		size_t loop_0      = 0;

		size_t loop_count;
		{
			auto t0 = Clock::now();
			O();
			auto t1    = Clock::now();
			loop_count = get_iterations((t1 - t0), 1);
		}

		auto t0 = Clock::now();
		do {
			O();
			loop_0++;
		} while (loop_0 < loop_count);
		auto t1                      = Clock::now();
		result.result.nanoseconds[0] = (t1 - t0).count();
		result.loops                 = loop_count + 1;

		return result;
	}
};