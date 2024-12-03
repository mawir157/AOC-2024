#ifdef DAY03

#include "AH.h"

namespace Day03
{

	std::pair<int, int> FindMul(std::string s)
	{
		int sum1 = 0, sum2 = 0;
		bool on = true;
		std::regex mul_regex("mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)");

		std::sregex_iterator iter(s.begin(), s.end(), mul_regex);
    	std::sregex_iterator end;

		std::smatch regex_match;
		while (std::regex_search (s,regex_match,mul_regex)) {
			auto p = regex_match.str(0);
			if (p == "do()") {
				on = true;
			} else if (p == "don't()") {
				on = false;
			} else { // mul(x,y)
				p = p.substr(4, p.size() - 2);
				auto ps = AH::Split(p, ',');
				int l = std::stoi(ps[0]);
				int r = std::stoi(ps[1]);

				sum1 += l*r;
				if (on) {
					sum2 += l*r;
				}
			}

			s = regex_match.suffix().str();
		}

		return std::make_pair(sum1, sum2);
	}

	int Run(const std::string& filename)
	{
		auto inputLines = AH::ReadTextFile(filename);
		auto s = std::accumulate(inputLines.begin(), inputLines.end(), std::string(""));
		auto [p1,p2] = FindMul(s);

		AH::PrintSoln(3, p1, p2);

		return 0;	
	}

}

#endif
