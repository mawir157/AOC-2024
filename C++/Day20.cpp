#ifdef DAY20

#include "AH.h"

namespace Day20
{

	struct Pos {
		int r,c;

		Pos(int r, int c) : r(r), c(c) {};
		Pos() : r(0), c(0) {};
		bool operator==(Pos const& obj) { return ((r == obj.r) && (c == obj.c)); }
		bool operator<(Pos const& obj) const {
			if (r != obj.r) 
				return r < obj.r;

			return c < obj.c;	
		}
	};

	enum CELL { SPACE, WALL };

	typedef std::vector<std::vector<CELL>> Grid;

	std::tuple<Grid, Pos, Pos> parseInput(
		const std::vector<std::string>ss
	)
	{
		Grid g;
		int r = 0;
		Pos start(0,0);
		Pos end(0,0);
		for (auto s : ss) {
			int c = 0;
			std::vector<CELL> row;
			for (auto ch : s) {
				if (ch == '#') {
					row.emplace_back(WALL);
				} else {
					row.emplace_back(SPACE);
					if (ch == 'S') {
						start.r = r;
						start.c = c;
					}
					if (ch == 'E') { 
						end.r = r;
						end.c = c;
					}
				}
				c++;
			}
			g.emplace_back(row);
			r++;
		}

		return std::make_tuple(g, start, end);
	}
 
	std::vector <Pos> nbhrs(const Pos p, const std::set<Pos> Q)
	{
		std::vector<Pos> ns;
		// N
		Pos pN(p.r - 1, p.c);
		if (Q.count(pN) != 0) {
			ns.emplace_back(pN);
		}
		// E
		Pos pE(p.r, p.c + 1);
		if (Q.count(pE) != 0) {
			ns.emplace_back(pE);
		}
		// S
		Pos pS(p.r + 1, p.c);
		if (Q.count(pS) != 0) {
			ns.emplace_back(pS);
		}
		// W
		Pos pW(p.r, p.c - 1);
		if (Q.count(pW) != 0) {
			ns.emplace_back(pW);
		}
	
		return ns;
	}

	Pos findMin(const std::set<Pos>Q, const std::map<Pos, int> M)
	{
		int min = 100000000;
		Pos min_p = Pos(-1,-1);
		for (auto p : Q) {
			if (M.at(p) < min) {
				min = M.at(p);
				min_p = p;
			}
		}
		return min_p;
	}

	std::map<Pos, int> dijkstra(const Grid g, const Pos start)
	{
		std::map<Pos, int> dist;
		std::set<Pos> Q;
		std::set<Pos> flagged;

		for (int r = 0; r < (int)g.size(); r++) {
			for (int c = 0; c < (int)g[r].size(); c++) {
				if (g[r][c] == SPACE) {
					Pos p(r,c);
					dist[p] = 1000000;
					Q.insert(p);
				}
			}
		}
		dist[start] = 0;
		flagged.insert(start);

		while (Q.size() > 0) {
			Pos minU = findMin(flagged, dist);
			if (minU == Pos(-1,-1)) {
				break;
			}
			Q.erase(minU);
			flagged.erase(minU);

			auto ns = nbhrs(minU, Q);
			for (auto n : ns) {
				auto d_step = 1;
				auto alt = dist[minU] + d_step;
				if (alt < dist[n]) {
					dist[n] = alt;
					flagged.insert(n);
				}
			}
		}

		return dist;
	}

	std::map<int, int> findShortCuts(
		const std::map<Pos, int> dist, 
		const int full_route,
		const int steps=2)
	{
		std::map<int, int> ss;

		// for each point on the path scan all (less than)
		// steps*steps possible end  points achievable via short cuts
		for (auto [p,d] : dist) {
			for (int dr = -steps; dr <= steps; dr++) {
				for (int dc = -steps; dc <= steps; dc++) {
					int dd = abs(dr) + abs(dc);
					if (dd > steps) {
						continue;
					}

					Pos shortcut(p.r + dr, p.c + dc);
					if (dist.count(shortcut) != 0) { // this cell is on the path
						const auto new_route = d + dd + (full_route - dist.at(shortcut));
						if (new_route < full_route) {
							ss[full_route - new_route]++;
						}
					}
				}
			}
		}

		return ss;
	}

	int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);

		auto [grid, start, end] = parseInput(is);	
		auto dist = dijkstra(grid, start);
		auto ss1 = findShortCuts(dist, dist.at(end));
		int p1 = 0;
		for (auto [p, c] : ss1) {
			p1 += (p >= 100) ? c : 0;
		}

		auto ss2 = findShortCuts(dist, dist.at(end), 20);
		int p2 = 0;
		for (auto [p, c] : ss2) {
			p2 += (p >= 100) ? c : 0;
		}

		AH::PrintSoln(20, p1, p2);
		return 0;
	}

}

#endif
