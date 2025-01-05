#ifdef DAY07

#include "AH.h"

namespace Day07
{

	std::pair<int64_t, std::vector<int64_t>> parseInput(const std::string s)
	{
		auto ps = AH::SplitOnString(s, ": ");
		int64_t t = AH::stoi64(ps[0]);
		std::vector<int64_t> arr;
		auto as = AH::SplitOnString(ps[1], " ");
		for (auto & a : as) {
			arr.emplace_back(AH::stoi64(a));
		}

		return std::make_pair(t, arr);
	}

	int64_t concatInts(int64_t x, int64_t y)
	{
		int64_t n = 0;
		int64_t yy = y;
		while (yy > 0) {
			n++;
			yy /= 10;
		}
		return x * AH::IntPow(10, n) + y;
	}

	bool countdown(
		int64_t acc,
		size_t ptr,
		std::pair<int64_t, std::vector<int64_t>> pr,
		bool part2=false)
	{
		auto target = pr.first;
		auto arr = pr.second;
		if (acc > target) {
			return false;
		}
		if (arr.size() == ptr) {
			return (acc == target);
		}

		auto x = arr[ptr];

		if (part2) {
			return countdown(acc + x, ptr+1, pr, true) || 
				   countdown(acc * x, ptr+1, pr, true) || 
				   countdown(concatInts(acc, x), ptr+1, pr, true);
		} else {
			return countdown(acc + x, ptr+1, pr) ||
				   countdown(acc * x, ptr+1, pr);
		}
	}

	int Run(const std::string& filename)
	{
		const auto ss = AH::ReadTextFile(filename);
		int64_t p1 = 0, p2 = 0;
		for (auto s : ss) {
			auto pr = parseInput(s);
			if (countdown(0,0,pr)) {
				p1 += pr.first;
			}
			if (countdown(0,0,pr,true)) {
				p2 += pr.first;
			}
		}


		AH::PrintSoln(8, p1, p2);

		return 0;
	}

}

#endif
