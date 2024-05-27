module;
import <cstdint>;
import <cstring>;
//#include <cstdint>
//#include <cstring>
#include "forceinline/forceinline.h"
export module wheathash;

/*
Wheathash takes (optimally) 32-bit inputs and produces a 64-bit hash as its result.
It is a slightly-edited version of Waterhash, which is an edited version of wyhash.
It is meant to use very similar code to Waterhash, which produces a 32-bit hash.
Original Author: Wang Yi <godspeed_china@yeah.net>
Wheathash Variant Author: Tommy Ettinger <tommy.ettinger@gmail.com>
Constexpr and Modularization: Alexander Anderson <anderson.john.alexander@gmail.com>
*/
constexpr uint64_t _wheatp0 = 0xa0761d6478bd642full, _wheatp1 = 0xe7037ed1a0b428dbull, _wheatp2 = 0x8ebc6af09c88c6e3ull;
constexpr uint64_t _wheatp3 = 0x589965cc75374cc3ull, _wheatp4 = 0x1d8e4e27c47d124full, _wheatp5 = 0xeb44accab455d165ull;

export {
	always_force_inline uint64_t _wheatmum(uint64_t A, uint64_t B) {
		uint64_t r = A * B;
		return r - (r >> 32);
	};

	always_force_inline uint64_t _wheatr08(const uint8_t* p) {
		uint8_t v;
		memcpy(&v, p, 1);
		return v;
	};

	always_force_inline uint64_t _wheatr16(const uint8_t* p) {
		uint16_t v;
		memcpy(&v, p, 2);
		return v;
	};

	always_force_inline uint64_t _wheatr32(const uint8_t* p) {
		uint32_t v;
		memcpy(&v, p, 4);
		return v;
	};

	inline uint64_t wheathash(const void* key, uint32_t len, uint64_t seed) {
		const uint8_t* p = (const uint8_t*)key;
		uint32_t	   i;
		for (i = 0; i + 16 <= len; i += 16, p += 16) {
			seed = _wheatmum(_wheatmum(_wheatr32(p) ^ _wheatp1, _wheatr32(p + 4) ^ _wheatp2) + seed,
							 _wheatmum(_wheatr32(p + 8) ^ _wheatp3, _wheatr32(p + 12) ^ _wheatp4));
		}
		seed += _wheatp5;
		switch (len & 15) {
			case 1:
				seed = _wheatmum(_wheatp2 ^ seed, _wheatr08(p) ^ _wheatp1);
				break;
			case 2:
				seed = _wheatmum(_wheatp3 ^ seed, _wheatr16(p) ^ _wheatp4);
				break;
			case 3:
				seed = _wheatmum(_wheatr16(p) ^ seed, _wheatr08(p + 2) ^ _wheatp2);
				break;
			case 4:
				seed = _wheatmum(_wheatr16(p) ^ seed, _wheatr16(p + 2) ^ _wheatp3);
				break;
			case 5:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr08(p + 4) ^ _wheatp1);
				break;
			case 6:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr16(p + 4) ^ _wheatp1);
				break;
			case 7:
				seed = _wheatmum(_wheatr32(p) ^ seed, (_wheatr16(p + 4) << 8 | _wheatr08(p + 6)) ^ _wheatp1);
				break;
			case 8:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp0);
				break;
			case 9:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^
				_wheatmum(seed ^ _wheatp4, _wheatr08(p + 8) ^ _wheatp3);
				break;
			case 10:
				seed =
				_wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^ _wheatmum(seed, _wheatr16(p + 8) ^ _wheatp3);
				break;
			case 11:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^
				_wheatmum(seed, ((_wheatr16(p + 8) << 8) | _wheatr08(p + 10)) ^ _wheatp3);
				break;
			case 12:
				seed =
				_wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^ _wheatmum(seed ^ _wheatr32(p + 8), _wheatp4);
				break;
			case 13:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^
				_wheatmum(seed ^ _wheatr32(p + 8), (_wheatr08(p + 12)) ^ _wheatp4);
				break;
			case 14:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^
				_wheatmum(seed ^ _wheatr32(p + 8), (_wheatr16(p + 12)) ^ _wheatp4);
				break;
			case 15:
				seed = _wheatmum(_wheatr32(p) ^ seed, _wheatr32(p + 4) ^ _wheatp2) ^
				_wheatmum(seed ^ _wheatr32(p + 8), (_wheatr16(p + 12) << 8 | _wheatr08(p + 14)) ^ _wheatp4);
				break;
		}
		seed = (seed ^ seed << 16) * (len ^ _wheatp0);
		return seed - (seed >> 31) + (seed << 33);
	};



	constexpr uint64_t wmum(uint64_t A, uint64_t B) {
		uint64_t r = A * B;
		return r - (r >> 32);
	};

	constexpr uint64_t weat08(const char* p) {
		uint64_t v = *p;
		return v;
	};

	constexpr uint64_t weat16(const char* p) {
		uint16_t v{};
		for (size_t i = 0; i < sizeof(uint16_t); i++)
		v |= ((uint16_t)p[i]) << (i * 8);
		return v;
	};

	constexpr uint64_t weat32(const char* p) {
		uint32_t v{};
		// memcpy(&v, p, 4);
		for (size_t i = 0; i < sizeof(uint32_t); i++)
		v |= ((uint32_t)p[i]) << (i * 8);
		return v;
	};

	constexpr uint64_t constexpr_wheathash(const char* key, uint32_t len, uint64_t seed) {
		const char* p = static_cast<const char*>(key);
		uint32_t	i;
		for (i = 0; i + 16 <= len; i += 16, p += 16) {
			seed = wmum(wmum(weat32(p) ^ _wheatp1, weat32(p + 4) ^ _wheatp2) + seed,
						wmum(weat32(p + 8) ^ _wheatp3, weat32(p + 12) ^ _wheatp4));
		}
		seed += _wheatp5;
		switch (len & 15) {
			case 1:
				seed = wmum(_wheatp2 ^ seed, weat08(p) ^ _wheatp1);
				break;
			case 2:
				seed = wmum(_wheatp3 ^ seed, weat16(p) ^ _wheatp4);
				break;
			case 3:
				seed = wmum(weat16(p) ^ seed, weat08(p + 2) ^ _wheatp2);
				break;
			case 4:
				seed = wmum(weat16(p) ^ seed, weat16(p + 2) ^ _wheatp3);
				break;
			case 5:
				seed = wmum(weat32(p) ^ seed, weat08(p + 4) ^ _wheatp1);
				break;
			case 6:
				seed = wmum(weat32(p) ^ seed, weat16(p + 4) ^ _wheatp1);
				break;
			case 7:
				seed = wmum(weat32(p) ^ seed, (weat16(p + 4) << 8 | weat08(p + 6)) ^ _wheatp1);
				break;
			case 8:
				seed = wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp0);
				break;
			case 9:
				seed = wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^
				wmum(seed ^ _wheatp4, weat08(p + 8) ^ _wheatp3);
				break;
			case 10:
				seed =
				wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^ wmum(seed, weat16(p + 8) ^ _wheatp3);
				break;
			case 11:
				seed = wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^
				wmum(seed, ((weat16(p + 8) << 8) | weat08(p + 10)) ^ _wheatp3);
				break;
			case 12:
				seed =
				wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^ wmum(seed ^ weat32(p + 8), _wheatp4);
				break;
			case 13:
				seed = wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^
				wmum(seed ^ weat32(p + 8), (weat08(p + 12)) ^ _wheatp4);
				break;
			case 14:
				seed = wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^
				wmum(seed ^ weat32(p + 8), (weat16(p + 12)) ^ _wheatp4);
				break;
			case 15:
				seed = wmum(weat32(p) ^ seed, weat32(p + 4) ^ _wheatp2) ^
				wmum(seed ^ weat32(p + 8), (weat16(p + 12) << 8 | weat08(p + 14)) ^ _wheatp4);
				break;
		}
		seed = (seed ^ seed << 16) * (len ^ _wheatp0);
		return seed - (seed >> 31) + (seed << 33);
	};
}