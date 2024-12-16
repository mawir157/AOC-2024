#ifdef DAY16

#include "AH.h"

namespace Day16
{

	struct Pos {
		int r,c,d; // d=0,1,2,3 == N,E,S,W

		Pos(int r, int c, int d) : r(r), c(c), d(d) {};
		Pos() : r(0), c(0), d(0) {};
		Pos operator+(Pos const& obj) const { return Pos(r + obj.r, c + obj.c, d); }
		// Pos operator-(Pos const& obj) { return Pos(r - obj.r, c - obj.c); }
		// bool operator!=(Pos const& obj) { return ((r != obj.r) || (c != obj.c)); }
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
        Pos start(0,0,0);
        Pos end(0,0,0);
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

    std::vector <Pos> nbhrs(const Grid g, const Pos p)
    {
        std::vector<Pos> ns;
        ns.emplace_back(p.r, p.c, (p.d + 3) % 4);
        ns.emplace_back(p.r, p.c, (p.d + 1) % 4);
        auto dir = p.d;
        Pos step(0,0,0);
        if (dir == 0)
            step.r -=1;
        if (dir == 1)
            step.c += 1; 
        if (dir == 2)
            step.r +=1;
        if (dir == 3)
            step.c -= 1;

        Pos new_p = p + step;
        if (g[new_p.r][new_p.c] == SPACE)
            ns.emplace_back(new_p);

        return ns;
    }

    Pos findMin(const std::set<Pos>Q, const std::map<Pos, int> M)
    {
        int min = 1000000;
        Pos min_p = Pos(0,0,0);
        for (auto p : Q) {
            if (M.at(p) < min) {
                min = M.at(p);
                min_p = p;
            }
        }
        return min_p;
    }

    int countBackTrack(
        const Pos p,
        std::map<Pos, std::vector<Pos>> m,
        std::set<Pos> & routes)
    {
        if (m.count(p) == 0) {
            auto cpy = p;
            cpy.d = 0;
            routes.insert(cpy);
        } else {
            for (auto pp : m[p]) {
                auto cpy = p;
                cpy.d = 0;
                routes.insert(cpy);
                countBackTrack(pp, m, routes);
            }
        }
        return (int)routes.size();
    }

    std::pair<int, int> dijkstra(const Grid g, const Pos start, const Pos end)
    {
        std::map<Pos, int> dist;
        std::map<Pos, std::vector<Pos>> prev;
        std::set<Pos> Q;
        std::set<Pos> flagged;

        for (int r = 0; r < (int)g.size(); r++) {
            for (int c = 0; c < (int)g[r].size(); c++) {
                for (int d = 0; d < 4; d++) {
                    if (g[r][c] == SPACE) {
                        Pos p(r,c,d);
                        dist[p] = 1000000;
                        Q.insert(p);
                    }
                }
            }
        }
        dist[start] = 0;
        flagged.insert(start);

        while (Q.size() > 0) {
            Pos minU = findMin(flagged, dist);
            Q.erase(minU);
            flagged.erase(minU);

            auto ns = nbhrs(g, minU);
            for (auto n : ns) {
                auto d_step = (n.d != minU.d) ? 1000 : 1;
                auto alt = dist[minU] + d_step;
                if (alt < dist[n]) {
                    dist[n] = alt;
                    prev[n] = { minU };
                    flagged.insert(n);
                } else if (alt == dist[n]) {
                    prev[n].emplace_back(minU);
                }
            }
        }

        std::set<Pos> routes;
        return std::make_pair(dist[end], countBackTrack(end, prev, routes));
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
