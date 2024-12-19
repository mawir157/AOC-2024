#ifdef DAY19

#include "AH.h"

namespace Day19
{

	typedef std::map<std::string, int64_t> memo ;

	int64_t match(
		const std::vector<std::string> & ts,
		const std::string & s,
		memo & m
	)
	{
		if (m.count(s)) { // we have see this towel before
			return m[s]; // if so we don't have to recalculate
		}

		int64_t count = 0;
		for (auto t : ts) {
			if (s.find(t) == 0) { // this towel is a prefix
				const auto new_s = s.substr(t.size()); // remove the prefix
				if (new_s.empty()) {
					count++; // we're done so increment the counter
				} else {
					count += match(ts, new_s, m); // lse RECUR BABY!
				}
			}
		}

		// we have counted how many ways to reach this string
		m[s] = count; // so cache it
		return count;
	}

	int Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		auto ts = AH::SplitOnString(is[0], ", ");
		std::vector<std::string> ps;
		for (int i = 2; i < (int)is.size(); i++) {
			ps.emplace_back(is[i]);
		}
		
		int64_t p1 = 0, p2 = 0;
		for (auto p : ps) {
			memo m;
			auto valid = match(ts, p, m);
			p1 += (valid > 0) ? 1 : 0;
			p2 += valid;
		}

		AH::PrintSoln(19, p1, p2);

		return 0;
	}

}

#endif
