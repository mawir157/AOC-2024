#ifdef DAY01

#include "AH.h"

namespace Day01
{

	void parseInput(
		const std::vector<std::string> & is,
		std::vector<int> & lhs,
		std::vector<int> & rhs
	)
	{
		lhs.reserve(is.size());
		rhs.reserve(is.size());
		for (auto & i : is) {
			const auto parts = AH::SplitOnString(i, "   ");
			lhs.push_back(std::stoi(parts[0]));
			rhs.push_back(std::stoi(parts[1]));
		}

		return;
	}

	int freq(
		const std::vector<int> & lhs,
		const std::vector<int> & rhs)
	{
		int s = 0;
		std::map<int, int> f;
		for (auto v : lhs)
		{
			if (f.count(v) == 0)
			{
				for (auto u : rhs) {
					if (u == v)
						f[v]++;
				}
			}
			s += v * f[v];
		}
		return s;
	}

	int Run(const std::string& filename)
	{
		const auto inputLines = AH::ReadTextFile(filename);
		std::vector<int> lhs, rhs;
		parseInput(inputLines, lhs, rhs);
		std::sort(lhs.begin(), lhs.end());
		std::sort(rhs.begin(), rhs.end());

		int part1 = 0;
		int part2 = freq(lhs, rhs);
		for (size_t i = 0; i <  inputLines.size(); i++)
		{
			part1 += std::abs(lhs[i] - rhs[i]);
		}

		AH::PrintSoln(1, part1, part2);

		return 0;
	}

}

#endif
