#ifdef DAY12

#include "AH.h"

namespace Day12
{

	typedef std::tuple<int, int, int> Region;
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
		std::vector<Region> rs;
		auto m_copy = m;
		std::list<Pos> q;
		while (m_copy.size() > 0)
		{ 
			auto p = m_copy.begin()->first; // 'random' starting point
			auto ch = m_copy.begin()->second;
			m_copy.erase(p);
			q.push_back(p);

			int r_size = 0;
			int r_peri = 0;
			int r_corners = 0;

			while(q.size() > 0) {
				r_size++;
				auto n = q.front();
				q.pop_front();
				bool n_f = false;
				bool e_f = false;
				bool s_f = false;
				bool w_f = false;

				// find nbrs
				auto North = Pos{.r=n.r-1, .c=n.c};
				if (m_copy.count(North) && m_copy[North] == ch) {
					m_copy.erase(North);
					q.push_back(North);
				} else if (m[North] != ch) {
					r_peri++;
					n_f = true;
				}

				auto East = Pos{.r=n.r, .c=n.c+1};
				if (m_copy.count(East) && m_copy[East] == ch) {
					m_copy.erase(East);
					q.push_back(East);
				} else if (m[East] != ch) {
					r_peri++;
					e_f = true;
				}

				auto South = Pos{.r=n.r+1, .c=n.c};
				if (m_copy.count(South) && m_copy[South] == ch) {
					m_copy.erase(South);
					q.push_back(South);
				} else if (m[South] != ch) {
					r_peri++;
					s_f = true;
				}

				auto West = Pos{.r=n.r, .c=n.c-1};
				if (m_copy.count(West) && m_copy[West] == ch) {
					m_copy.erase(West);
					q.push_back(West);
				} else if (m[West] != ch) {
					r_peri++;
					w_f = true;
				}

				
				// corner counting!
				// +ve corners
				if (n_f && e_f) {
					r_corners++;
				}
				if (e_f && s_f) {
					r_corners++;
				}
				if (s_f && w_f) {
					r_corners++;
				}
				if (w_f && n_f) {
					r_corners++;
				}

				// -ve corners
				if (!(n_f || e_f)) { // not blocked to north or east
					auto NE = Pos{.r=n.r-1, .c=n.c+1};
					if (m[NE] != ch) { // this is a different region...
						r_corners++; // .. so this is a corner
					}
				}
				if (!(e_f || s_f)) { // not blocked to east or south
					auto SE = Pos{.r=n.r+1, .c=n.c+1};
					if (m[SE] != ch) { // this is a different region...
						r_corners++; // .. so this is a corner
					}
				}
				if (!(s_f || w_f)) { // not blocked to south or west
					auto SW = Pos{.r=n.r+1, .c=n.c-1};
					if (m[SW] != ch) { // this is a different region...
						r_corners++; // .. so this is a corner
					}
				}
				if (!(w_f || n_f)) { // not blocked to west or north
					auto NW = Pos{.r=n.r-1, .c=n.c-1};
					if (m[NW] != ch) { // this is a different region...
						r_corners++; // .. so this is a corner
					}
				}
			}
			rs.emplace_back(r_size, r_peri, r_corners);
		}

		return rs;
	}
	
	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		auto m = parseInput(is);
		auto pairs = part1(m);
		int p1 = 0;
		int p2 = 0;
		for (auto [a,p,w] : pairs) {
			p1 += a*p;
			p2 += a*w;
		}

		AH::PrintSoln(12, p1, p2);
		return 0;
	}
}

#endif