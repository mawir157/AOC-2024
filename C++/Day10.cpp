#ifdef DAY10

#include "AH.h"

namespace Day10
{

	typedef std::vector<std::vector<int>> Grid;
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

	std::pair<Grid, std::vector<Pos>> parseInput(const std::vector<std::string> & ss)
	{
		Grid grid;
		std::vector<Pos> starts;

		for (int r = 0; r < (int)ss.size(); r++) {
			std::vector<int> row;
			for (int c = 0; c < (int)ss[0].size(); c++) {
				row.emplace_back(int(ss[r][c] - '0'));
				if (ss[r][c] == '0') {
					starts.emplace_back(Pos{.r=r, .c=c});
				}
			}
			grid.emplace_back(row);
		}

		return std::make_pair(grid, starts);
	}

	std::vector<Pos> nhbrs(const Pos p, const Grid g)
	{
		const auto cur = g[p.r][p.c];
		const int lim_r = (int)g.size();
		const int lim_c = (int)g[0].size();
		std::vector<Pos> ns;
		if ((p.r-1 >= 0) && (g[p.r-1][p.c] == cur +1)) {
			ns.emplace_back(Pos{.r=p.r-1, .c=p.c});
		}
		if ((p.c-1 >= 0) && (g[p.r][p.c-1] == cur +1)) {
			ns.emplace_back(Pos{.r=p.r, .c=p.c-1});
		}
		if ((p.r+1 < lim_r) && (g[p.r+1][p.c] == cur +1)) {
			ns.emplace_back(Pos{.r=p.r+1, .c=p.c});
		}
		if ((p.c+1 < lim_c) && (g[p.r][p.c+1] == cur +1)) {
			ns.emplace_back(Pos{.r=p.r, .c=p.c+1});
		}

		return ns;
	}

	void ends(const Grid g, const Pos p, std::map<Pos, int> & es)
	{
		int cur = g[p.r][p.c];

		if (cur == 9) {
			es[p]++;
			return;
		}

		const auto ns = nhbrs(p, g);
		for (auto n : ns) {
			ends(g, n, es);
		}

		return;
	}
	
	int Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		const auto [grid, starts] = parseInput(is);

		int part1 = 0;
		int part2 = 0;
		std::map<Pos, int> es;
		for (auto s : starts) {
			es.clear();
			ends(grid, s, es);
			part1 += es.size();
			for (auto [_,v] : es) {
				part2 += v;
			}
		}

		AH::PrintSoln(10, part1, part2);
		return 0;
	}
}

#endif
