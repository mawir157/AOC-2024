#ifdef DAY16

#include "AH.h"

namespace Day16
{

	struct Pos {
		int r=0,c=0,d=0; // d=0,1,2,3 == N,E,S,W

		// Pos(int r, int c, int d) : r(r), c(c), d(d) {};
		// Pos() : r(0), c(0), d(0) {};
		Pos operator+(Pos const& obj) const { return Pos{r + obj.r, c + obj.c, d}; }
		// Pos operator-(Pos const& obj) { return Pos(r - obj.r, c - obj.c); }
		// bool operator!=(Pos const& obj) { return ((r != obj.r) || (c != obj.c)); }
		bool operator==(Pos const& obj) { return ((r == obj.r) && (c == obj.c)); }
		bool operator<(Pos const& obj) const {
			if (r != obj.r) 
				return r < obj.r;

			if (c != obj.c)
				return c < obj.c;
				
			return (d < obj.d);
		}
	};

	enum CELL { SPACE, WALL };

	typedef std::vector<std::vector<CELL>> Grid;

	std::tuple<Grid, Pos, Pos> parseWarehouse(
		const std::vector<std::string>ss
	)
	{
		Grid g;
		int r = 0;
		Pos start{0,0,0};
		Pos end{0,0,0};
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
						start.d = 1;
					}
					if (ch == 'E') { 
						end.r = r;
						end.c = c;
						end.d = 0;
					}
				}
				c++;
			}
			g.emplace_back(row);
			r++;
		}

		return std::make_tuple(g, start, end);
	}


	std::vector <Pos> nbhrs(
		const Grid g,
		const Pos p,
		const std::vector<std::vector<std::vector<bool>>> & Q)
	{
		std::vector<Pos> ns;
		Pos p_pos{p.r, p.c, (p.d + 3) % 4};
		if (Q[p_pos.r][p_pos.c][p_pos.d]) {
			ns.emplace_back(p_pos);
		}

		Pos p_neg{p.r, p.c, (p.d + 1) % 4};
		if (Q[p_neg.r][p_neg.c][p_neg.d]) {
			ns.emplace_back(p_neg);
		}
	
		auto dir = p.d;
		Pos step{0,0, 0};
		if (dir == 0)
			step.r -=1;
		if (dir == 1)
			step.c += 1; 
		if (dir == 2)
			step.r +=1;
		if (dir == 3)
			step.c -= 1;

		Pos new_p = p + step;
		if ((g[new_p.r][new_p.c] == SPACE) && (Q[new_p.r][new_p.c][new_p.d])) {
			ns.emplace_back(new_p);
		}

		return ns;
	}

	void countBackTrack(
		const Pos p,
		const std::map<Pos, std::vector<Pos>> & prev,
		std::vector<std::vector<bool>> & routes)
	{	
		routes[p.r][p.c] = true;
	
		if (prev.count(p) == 0) {
			return;
		} else {
			for (auto pp : prev.at(p)) {
					routes[p.r][p.c] = true;
					countBackTrack(pp, prev, routes);
			}
		}


		return;
	}

	std::pair<int, int> dijkstra(const Grid g, const Pos start, const Pos end)
	{
		std::vector<std::vector<std::vector<int>>> dist;
		std::map<Pos, std::vector<Pos>> prev;
		std::vector<std::vector<std::vector<bool>>> Q;
		std::list<std::pair<Pos, int>> flagged;

		dist.resize(g.size());
		Q.resize(g.size());
		for (int r = 0; r < (int)g.size(); r++) {
			dist[r].resize(g[r].size());
			Q[r].resize(g[r].size());
			for (int c = 0; c < (int)g[r].size(); c++) {
				dist[r][c].resize(4);
				Q[r][c].resize(4);
				for (int d = 0; d < 4; d++) {
					if (g[r][c] == SPACE) {
						dist[r][c][d] = 1000000;
						Q[r][c][d] = true;
					} 
				}
			}
		}
		dist[start.r][start.c][start.d] = 0;
		int alt = 0;
		flagged.emplace_back(start, 0);

		while (flagged.size() > 0) {
			auto [minU, _ ] = flagged.front();
			Q[minU.r][minU.c][minU.d]= false;
			flagged.pop_front();

			auto ns = nbhrs(g, minU, Q);
			for (auto n : ns) {
				auto d_step = (n.d != minU.d) ? 1000 : 1;
				alt = dist[minU.r][minU.c][minU.d] + d_step;
				if (alt < dist[n.r][n.c][n.d]) {
					dist[n.r][n.c][n.d] = alt;
					prev[n] = { minU };

					auto pair = std::make_pair(n, alt);
					bool at_end = true;
					for (auto ptr = flagged.begin(); ptr != flagged.end(); ptr++) {
						if (alt < (*ptr).second) {
							flagged.insert(ptr, pair);
							at_end = false;
							break;
						}
					}
					if (at_end) {
						flagged.emplace_back(pair);
					}

				} else if (alt == dist[n.r][n.c][n.d]) {
					prev[n].emplace_back(minU);
				}
			}
		}

		std::vector<std::vector<bool>> routes;
		routes.resize(g.size());
		for (auto & r : routes) {
			r.resize(g.size());
		}

		countBackTrack(end, prev, routes);
		int counter = 0;
		for (auto r : routes) {
			for (auto c : r) {
				counter += c ? 1 : 0;
			}
		}

		return std::make_pair(
			dist[end.r][end.c][end.d],
			counter
		);
	}

	int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		auto [grid, start, end] = parseWarehouse(is);	

		auto [p1, p2] = dijkstra(grid, start, end);

		AH::PrintSoln(16, p1, p2);
		return 0;
	}
}

#endif
