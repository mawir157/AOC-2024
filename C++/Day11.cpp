#ifdef DAY11

#include "AH.h"

namespace Day11
{
	
	std::map<uint64_t, uint64_t> parseInput(const std::string s)
	{
		std::map<uint64_t, uint64_t> arr;
		auto ps = AH::Split(s, ' ');
		for (auto p : ps) {
			auto v = AH::stoi64(p);
			arr[v]++;
		}

		return arr;
	}

	uint64_t n_digits(uint64_t x)
	{
		uint64_t n = 0;
		while (x > 0) {
			n++;
			x /= 10;
		}
		return n;
	}

	std::map<uint64_t, uint64_t> blink(const std::map<uint64_t, uint64_t> m)
	{
		std::map<uint64_t, uint64_t> arr;
		for (auto [v, c] : m) {
			if (v == 0) {
				arr[1] += c;
				continue;;
			}

			auto n = n_digits(v);
			if (n % 2 == 0) {
				auto tens = AH::IntPow(10, n/2);
				uint64_t l = v / tens;
				uint64_t r = v % tens;
				arr[l] += c;
				arr[r] += c;
				continue;
			}

			arr[2024 * v] += c;
		}

		return arr;
	}

	uint64_t blinkAgain(const int n, const std::map<uint64_t, uint64_t> m)
	{
		std::map<uint64_t, uint64_t> mm = m;
		for (int i = 0; i < n; i++) {
			mm = blink(mm);
		}

		uint64_t count = 0;
		for (auto [_, v] : mm) {
			count += v;
		}

		return count;
	}

	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		auto m = parseInput(is[0]);
		uint64_t p1 = blinkAgain(25, m);
		uint64_t p2 = blinkAgain(75, m);

		// std::cout << p2 << "\n";

		AH::PrintSoln(11, p1, p2);
		return 0;
	}
}

#endif
