#ifdef DAY08

#include "AH.h"

namespace Day08
{

	typedef std::vector<std::string> Grid;
	struct Pos {
		int r, c;

		bool operator<(const Pos& other) const
		{
			if (r != other.r) { return r < other.r ;}
			if (c != other.c) { return c < other.c ;}
			return false; 
		}
	};
	
	std::map<char, std::vector<Pos>> findAntennae(const Grid g) {
		std::map<char, std::vector<Pos>> as;
		for (int r = 0; r < (int)g.size(); r++) {
			for (int c = 0; c < (int)g[r].size(); c++) {
				const auto ch = g[r][c];
				if (ch == '.') {
					continue;
				}
				Pos p;
				p.r = r; p.c=c;
				as[ch].emplace_back(p);
			}
		}
		return as;
	}

	std::pair<Pos, Pos> nodePair(const Pos p1, const Pos p2) {
		const auto dr = p1.r - p2.r;
		const auto dc = p1.c - p2.c;

		const Pos n1{.r=p1.r+dr, .c=p1.c+dc};
		const Pos n2{.r=p2.r-dr, .c=p2.c-dc};

		return std::make_pair(n1, n2);
	}

	std::set<Pos> nodeLines(const Grid g, const Pos p1, const Pos p2) {
		std::set<Pos> ns;

		const int r_max = g.size();
		const int c_max = g[0].size();

		const auto dr = p1.r - p2.r;
		const auto dc = p1.c - p2.c;

		auto nd =  p1;
		while  ((nd.r >=0) && (nd.r < r_max) && (nd.c >=0) && (nd.c < c_max)) {
			ns.emplace(nd);
			nd.r -= dr;
			nd.c -= dc;
		}


		nd =  p1;
		while  ((nd.r >=0) && (nd.r < r_max) && (nd.c >=0) && (nd.c < c_max)) {
			ns.emplace(nd);
			nd.r += dr;
			nd.c += dc;
		}

		return ns;
	}

	std::pair<int, int> findAntiNodes( const Grid g, const std::map<char, std::vector<Pos>> as) {
		std::set<Pos> nodes1;
		std::set<Pos> nodes2;
		const int r_max = g.size();
		const int c_max = g[0].size();
		for (auto & [a, ps] : as) {
			if (ps.size() < 2) {
				continue;
			}
			for (int p1 = 0; p1 < (int)ps.size() - 1; p1++) {
				const auto pos1 = ps[p1];
				for (int p2 = p1 + 1; p2 < (int)ps.size(); p2++) {
					const auto pos2 = ps[p2];


					const auto [n1, n2] = nodePair(pos1, pos2);
					if ((n1.r >=0) && (n1.r < r_max) && (n1.c >=0) && (n1.c < c_max)) {
						nodes1.insert(n1);
					}
					if ((n2.r >=0) && (n2.r < r_max) && (n2.c >=0) && (n2.c < c_max)) {
						nodes1.insert(n2);
					}

					const auto nodes = nodeLines(g, pos1, pos2);
					nodes2.insert(nodes.begin(), nodes.end());

				}
			}
		}

		return std::make_pair( (int)nodes1.size(), (int)nodes2.size() );
	}

	int Run(const std::string& filename)
	{
		const auto g = AH::ReadTextFile(filename);
		const auto as = findAntennae(g);
		const auto [p1, p2] = findAntiNodes(g, as);

		AH::PrintSoln(7, p1, p2);

		return 0;
	}

}

#endif
