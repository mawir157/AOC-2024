#ifdef DAY14

#include "AH.h"

namespace Day14
{

	typedef std::pair<int, int> Pos;
	
	struct Point {
		int x,y,dx,dy;
	};

	std::pair<int, int> afterNSteps(
		const int n,
		const Point p,
		const int wrap_x=101,
		const int wrap_y=103
	)
	{
		int xx = ((p.x + n * p.dx) % wrap_x + wrap_x) % wrap_x;
		int yy = ((p.y + n * p.dy) % wrap_y + wrap_y) % wrap_y;

		return std::make_pair(xx, yy);
	}

	void quadCount(
		std::vector<int> & v,
		const std::pair<int, int> p,
		const int wrap_x=101,
		const int wrap_y=103
	)
	{
		if (p.first < wrap_x / 2) {
			if (p.second < wrap_y / 2) {
				v[0]++;
			}  else if (p.second > wrap_y / 2) {
				v[1]++;
			}
		}  else if (p.first > wrap_x / 2) {
			if (p.second < wrap_y / 2) {
				v[2]++;
			}  else if (p.second > wrap_y / 2) {
				v[3]++;
			}
		}

		return;
	}

	Point parseInput(const std::string s)
	{
		Point p;
		size_t from = 0;
		size_t to = 0;
		std::string number = "";
		// x
		from = s.find("=", from);
		to = s.find(",", to);
		number = s.substr (from+1,to);
		p.x = std::stoi(number);
		// y
		from = s.find(",", from);
		to = s.find(" ", to);
		number = s.substr (from+1,to);
		p.y = std::stoi(number);
		// dx
		from = s.find("=", from);
		to = s.find(",", to);
		number = s.substr (from+1,to);
		p.dx = std::stoi(number);
		// dy
		from = s.find(",", from);
		to = s.find(" ", to);
		number = s.substr (from+1,to);
		p.dy = std::stoi(number);

		return p;
	}

	void plotStars(
		const std::set<Pos> s,
		const int wrap_x=101,
		const int wrap_y=103
	)
	{
		for (int y = 0; y < wrap_y; y++) {
			for (int x = 0; x < wrap_x; x++) {
				auto p = std::make_pair(x,y);
				if (s.count(p) == 0) {
					std::cout << ".";
				} else {
					std::cout << "#";
				}
			}
			std::cout << "\n";
		}

		return;
	}

	int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		std::vector<Point> ps;
		std::vector<int> quads(4, 0);
		for (auto i : is) {
			auto p = parseInput(i);
			ps.emplace_back(p);
			auto s = afterNSteps(100, p);
			quadCount(quads, s);
		}
		int p1 = quads[0] * quads[1] * quads[2] * quads[3]; 
		int p2 = 0;
		for (p2 = 101*103; p2 > 0; p2--) {
			// std::set<Pos> s;
			// count the number of Points per row/col
			std::vector<int> row_count(103,0);
			std::vector<int> col_count(103,0);
			for (auto p : ps) {
				auto pn = afterNSteps(p2, p);
				// s.emplace(pn);
				row_count[pn.first]++;
				col_count[pn.second]++;
			}

			int most_row = 0;
			for (auto rc : row_count) {
				if (rc > most_row) {
					most_row = rc;
				}
			}
			int most_col = 0;
			for (auto cc : col_count) {
				if (cc > most_col) {
					most_col = cc;
				}
			}
			// this is probably the christmas tree
			if ((most_row > 30) && (most_col > 30)) { 
				//plotStars(s);
				break;
			} 
		}

		AH::PrintSoln(14, p1, p2);
		return 0;
	}
}

#endif
