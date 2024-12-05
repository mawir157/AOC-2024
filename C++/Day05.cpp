#ifdef DAY05

#include "AH.h"

namespace Day05
{

	typedef std::pair<int,int> Rule;
	
	std::vector<Rule> parseRules(const std::string s) {
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

	std::vector<std::vector<int>> parseBook(const std::string s) {
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

	bool isThisBookAnyGood(
		const std::vector<Rule> rs,
		const std::vector<int> b
	)
	{
		for (size_t i = 0; i < b.size() - 1; i ++) {
			const auto p1 = b[i];
			for (size_t j = i + 1; j < b.size(); j++) {
				const auto p2 = b[j];

				// does this breach a rule?
				for (auto r : rs) {
					if ((r.first == p2) && (r.second == p1)) {
						return false;
					}
				}
			}
		}
		
		return true;
	}

    std::vector<int> sortBook (
		const std::vector<Rule> rs,
		const std::vector<int> b
	)
    {
        std::vector<int>new_book = b;
        // do n bubble sorts cause i am an scrub
        for (size_t iters = 0; iters < b.size(); ++iters) {
            for (auto  p1 = new_book.begin(); p1 != new_book.end() - 1; p1++) {
                for (auto p2 = p1 + 1; p2 != new_book.end(); p2++) {

                    // does this breach a rule?
                    for (auto r : rs) {
                        if ((r.first == *p2) && (r.second == *p1)) {
                            std::iter_swap(p1, p2);
                            break;
                        }
                    }
                }
            }           
        }

        return new_book;
    }

	int Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		const auto ss = AH::ParseLineGroups(is ,'#');
		const auto rules = parseRules(ss[0]);
		const auto books = parseBook(ss[1]);

        int part1 = 0;
        int part2 = 0;
		for (auto book : books) {
			if (isThisBookAnyGood(rules, book)) {
                part1 += book[book.size() / 2];
			} else {
                auto fixed_book = sortBook(rules, book);
                part2 += fixed_book[fixed_book.size() / 2];
            }
		}

		AH::PrintSoln(5, part1, part2);

		return 0;
	}

}

#endif
