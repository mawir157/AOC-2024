#ifdef DAY02

#include "AH.h"

namespace Day02
{

	std::vector<std::vector<int>> parseInput(const std::vector<std::string> & is)
	{
		std::vector<std::vector<int>> ss;
		ss.reserve(is.size());
		for (auto & i : is) {
			const auto ps = AH::SplitOnString(i, " ");
			std::vector<int> s;
			for (auto & p : ps) {
				s.push_back(std::stoi(p));
			}
			
			ss.push_back(s);
		}

		return ss;
	}

	bool analyse(const std::vector<int> & vs)
	{
		std::vector<int> diff;
		for (size_t i = 0; i < vs.size() - 1; i++) {
			diff.emplace_back(vs[i+1] - vs[i]);
		}

		const int pos = diff[0] > 0;
		for (auto & v: diff) {
			if ((v > 0) != pos) {
				return false;
			}
			if ((std::abs(v) < 1) || (std::abs(v) > 3)) {
				return false;
			}
		}

		return true;
	}

	bool analyse2(const std::vector<int> & vs)
	{
		for (size_t n = 0; n < vs.size(); n++) {
			std::vector<int> seqNew;
			seqNew.reserve(vs.size() - 1);
			for (size_t i = 0; i < vs.size(); i++) {
				if (i == n) {
					continue;
				}
				seqNew.emplace_back(vs[i]);
			}
			if (analyse(seqNew)) {
				return true;
			}
		}

		return false;
	}

	int Run(const std::string& filename)
	{
		const auto inputLines = AH::ReadTextFile(filename);
		auto ss = parseInput(inputLines);

		int part1 = 0;
		int part2 = 0;
		for (auto & s : ss)
		{
			if (analyse(s)) {
				part1++;
				part2++;
			} else {
				if (analyse2(s)) {
					part2++;
				}
			}
		}

		AH::PrintSoln(2, part1, part2);

		return 0;
	}

}

#endif
