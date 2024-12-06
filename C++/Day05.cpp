#ifdef DAY05

#include "AH.h"

namespace Day05
{

	typedef std::pair<int,int> Rule;
	
	std::vector<Rule> parseRules(const std::string s)
	{
		std::vector <Rule> rs;
		const auto ss = AH::Split(s, '#');

		for (auto r : ss) {
			const auto ps = AH::Split(r, '|');
			auto lhs = std::stoi(ps[0]);
			auto rhs = std::stoi(ps[1]);
			rs.emplace_back(lhs, rhs);
		}

		return rs;
	}

	std::vector<std::vector<int>> parseBooks(const std::string s)
	{
		std::vector <std::vector<int>> bs;
		const auto ss = AH::Split(s, '#');
		for (auto r : ss) {
			std::vector<int> b;
			const auto ps = AH::Split(r, ',');
			for (auto p : ps) {
				auto pg = std::stoi(p);
				b.emplace_back(pg);
			}
			bs.emplace_back(b);
		}

		return bs;
	}

	std::pair<std::vector<int>, int> sortBook (
		const std::vector<Rule> rs,
		const std::vector<int> b
	)
	{
		std::vector<int>new_book = b;
		int counter = 0;
		// do n bubble sorts cause i am an scrub
		for (size_t iters = 0; iters < b.size(); ++iters) {
			for (auto  p1 = new_book.begin(); p1 != new_book.end() - 1; p1++) {
				for (auto p2 = p1 + 1; p2 != new_book.end(); p2++) {

					// does this breach a rule?
					for (auto r : rs) {
						if ((r.first == *p2) && (r.second == *p1)) {
							std::iter_swap(p1, p2);
							counter++;
						}
					}
				}
			}		   
		}

		return std::make_pair(new_book, counter);
	}

	int Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		const auto ss = AH::ParseLineGroups(is ,'#');
		const auto rules = parseRules(ss[0]);
		const auto books = parseBooks(ss[1]);

		int part1 = 0;
		int part2 = 0;
		for (auto book : books) {
			auto [ordered, count] = sortBook(rules, book);
			if (count == 0) {
				part1 += ordered[book.size() / 2];
			} else {
				part2 += ordered[book.size() / 2];
			}
		}

		AH::PrintSoln(5, part1, part2);

		return 0;
	}

}

#endif
