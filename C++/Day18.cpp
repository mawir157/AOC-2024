#ifdef DAY18

#include "AH.h"

namespace Day18
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

	Grid parseInput(std::vector<std::string> ss, int kb)
	{
		Grid g;
		g.resize(71);
		for (int i = 0; i < (int)g.size(); i++) {
			g[i].resize(71);
		}
		int counter = 0;
		for (auto s : ss) {
			auto ps = AH::Split(s, ',');
			auto r = std::stoi(ps[0]);
			auto c = std::stoi(ps[1]);

			g[r][c] = WALL;
			counter++;
			if (counter >= kb) {
				break;
			}
		}

		return g;
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

	int dijkstra(const Grid g, const Pos start, const Pos end)
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

		return dist[end];
	}

	int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		auto g = parseInput(is, 1024);
		int p1 = dijkstra(g, Pos(0,0), Pos(70,70));

		int kbs = is.size() - 1;
		for (; kbs >= 0; kbs--) {
			auto g = parseInput(is, kbs);
			auto test = dijkstra(g, Pos(0,0), Pos(70,70));
			if (test != 1000000) {
				break;
			}
		}
		
		AH::PrintSoln(18, p1, is[kbs]);
		return 0;
	}

}

#endif
