#ifdef DAY12

#include "AH.h"

namespace Day12
{

	typedef std::pair<int, int> Region;
	struct Pos {
		int r;
		int c;

		bool operator<(const Pos& other) const
		{
			if (r != other.r) { return r < other.r ;}
			if (c != other.c) { return c < other.c ;}
			return false; 
		}
	};

	std::map<Pos, char> parseInput(std::vector<std::string> ss)
	{
		std::map<Pos, char> m;
		for (int r = 0; r < (int)ss.size(); r++) {
			for (int c = 0; c < (int)ss.size(); c++) {
				auto ch = ss[r][c];
				auto p = Pos{.r=r, .c=c};
				m[p] = ch;
			}
		}

		return m;
	}

	std::vector<Region> part1(std::map<Pos, char> m)
	{
		std::vector<std::pair<int, int>> rs;
		auto m_copy = m;
		std::list<Pos> q;
		while (m_copy.size() > 0)
		{ 
			auto p = m_copy.begin()->first; // random starting point
			auto ch = m_copy.begin()->second;
			m_copy.erase(p);
			q.push_back(p);

			int r_size = 0;
			int r_peri = 0;
			int r_walls = 0;

			std::cout << ch << " - ";

			while(q.size() > 0) {
				r_size++;
				auto n = q.front();
				q.pop_front();
				int corners = 0;

				// find nbrs
				auto North = Pos{.r=n.r-1, .c=n.c};
				if (m_copy.count(North) && m_copy[North] == ch) {
					m_copy.erase(North);
					q.push_back(North);
				} else if (m[North] != ch) {
					r_peri++;
					corners++;
				}

				auto East = Pos{.r=n.r, .c=n.c+1};
				if (m_copy.count(East) && m_copy[East] == ch) {
					m_copy.erase(East);
					q.push_back(East);
				} else if (m[East] != ch) {
					r_peri++;
					corners++;
				}

				auto South = Pos{.r=n.r+1, .c=n.c};
				if (m_copy.count(South) && m_copy[South] == ch) {
					m_copy.erase(South);
					q.push_back(South);
				} else if (m[South] != ch) {
					r_peri++;
					corners++;
				}

				auto West = Pos{.r=n.r, .c=n.c-1};
				if (m_copy.count(West) && m_copy[West] == ch) {
					m_copy.erase(West);
					q.push_back(West);
				} else if (m[West] != ch) {
					r_peri++;
					corners++;
				}

				std::cout << "+" << (corners - 1) << " ";
				r_walls += (corners - 1);
			}
			rs.emplace_back(r_size, r_peri);
			std::cout << r_walls << "\n";
		}

		return rs;
	}
	
	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		auto m = parseInput(is);
		auto pairs = part1(m);
		int p1 = 0;
		for (auto [a,p] : pairs) {
			p1 += a*p;
		}
		int p2 = 0;

		AH::PrintSoln(12, p1, p2);
		return 0;
	}
}

#endif