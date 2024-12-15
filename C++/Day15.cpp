#ifdef DAY15

#include "AH.h"

namespace Day15
{
	enum CELL { SPACE, BOX, WALL, BOXL, BOXR };
	
	struct Pos {
		int r,c;

		Pos(int r, int c) : r(r), c(c) {};
		Pos operator+(Pos const& obj) { return Pos(r + obj.r, c + obj.c); }
		Pos operator-(Pos const& obj) { return Pos(r - obj.r, c - obj.c); }
		bool operator!=(Pos const& obj) { return ((r != obj.r) || (c != obj.c)); }
		bool operator<(Pos const& obj)const {
			if (r != obj.r) 
				return r < obj.r;
				
			return (c < obj.c);
		}
	};
	
	typedef std::vector<std::vector<CELL>> Warehouse;
	typedef Pos Robot;

	std::pair<Warehouse, Robot> parseWarehouse(
		const std::string s,
		const bool part1=true)
	{
		Warehouse w;
		Robot r(0,0);
		int ri = 0;
		auto input_rows = AH::Split(s, '|');
		for (auto input_row : input_rows) {
			int ci =0;
			std::vector<CELL> row;
			for (auto c : input_row) {
				if (c == '.') {
					if (part1) {
						row.emplace_back(SPACE);
					} else {
						row.emplace_back(SPACE);
						row.emplace_back(SPACE);
					}
				} else if (c == 'O') {
					if (part1) {
						row.emplace_back(BOX);
					} else {
						row.emplace_back(BOXL);
						row.emplace_back(BOXR);
					}
				} else if (c == '#') {
					if (part1) {
						row.emplace_back(WALL);
					} else {
						row.emplace_back(WALL);
						row.emplace_back(WALL);
					}
				} else if (c == '@') {
					r.r = ri;
					if (part1) {
						r.c = ci;
						row.emplace_back(SPACE);
					} else {
						r.c = 2*ci;
						row.emplace_back(SPACE);
						row.emplace_back(SPACE);
					}  
				}

				ci++;
			}
			w. emplace_back(row);
			ri++;
		}

		return std::make_pair(w,r);
	}

	bool moveRobot(Robot & r, Warehouse & w, const char c)
	{
		Pos step = Pos(0,0);
		if (c == '<') {
			step = Pos(0,-1);
		} else if (c == '^') {
			step = Pos(-1,0);
		} else if (c == '>') {
			step = Pos(0,1);
		} else if (c == 'v') {
			step = Pos(1,0);
		} else {
			return false;
		}

		auto new_r = r + step;
		auto cell = w[new_r.r][new_r.c];

		if (cell == WALL) {
			return false;
		}
		if (cell == SPACE) {
			r = new_r;
			return true;
		}
		if (cell == BOX) {
			auto box = new_r;
			auto cell_end = w[box.r][box.c];
			while (cell_end == BOX) {
				box = box + step;
				cell_end = w[box.r][box.c];
			}
			if (cell_end == WALL) {
				return false;
			}
			if (cell_end == SPACE) {
				std::swap(w[new_r.r][new_r.c], w[box.r][box.c]);
				r = new_r;
				return true;
			}
		}

		if ((cell == BOXL) || (cell == BOXR)) {
			if ((c == '<') || (c == '>')) { // moving EW is 'easy'
				auto box = new_r;
				auto cell_end = w[box.r][box.c];
				while ((cell_end == BOXL) || (cell_end == BOXR)) {
					box = box + step;
					cell_end = w[box.r][box.c];
				}
				if (cell_end == WALL) {
					return false;
				}
				if (cell_end == SPACE) {
					while (box != new_r) {
						auto box_m = box - step;
						std::swap(w[box_m.r][box_m.c], w[box.r][box.c]);
						box = box_m;
					}
					r = new_r;
					return true;
				}
			} else { // pushing the blocks NS
				std::set<Pos> to_move; // these are the cells we are moving
				std::set<Pos> active;
				to_move.insert(new_r);
				active.insert(new_r);
				if (cell == BOXL) { // we are pushing the left side of the box
					to_move.insert(new_r + Pos(0,1)); // so add the right
					active.insert(new_r + Pos(0,1)); // so add the right
				} else {
					to_move.insert(new_r - Pos(0,1)); // else add the left
					active.insert(new_r - Pos(0,1)); // else add the left
				}
				// is there space to move the move the blocks to ?
				bool blocked = false;
				while ((active.size() > 0) && !blocked) {
					std::set<Pos> next_active;
					for (auto p : active) {
						auto pp = p + step;
						auto cell_ns = w[pp.r][pp.c];

						if (cell_ns == WALL) {
							blocked = true;
							break;
						}

						if (cell_ns == SPACE) {
							// do nothing
							continue;
						}

						// we've hit a brick
						to_move.insert(pp);
						next_active.insert(pp);

						if (cell_ns == BOXL) {
							to_move.insert(pp + Pos(0,1));
							next_active.insert(pp + Pos(0,1)); 
						} else {
							to_move.insert(pp - Pos(0,1));
							next_active.insert(pp - Pos(0,1));
						}
					}
					active = next_active;
				}
				if (blocked) {
					to_move.clear();
					return false;
				}
				// at this point we have a set of boxes to move
				std::vector<Pos> move_ordered;
				for (auto p : to_move) {
					move_ordered.emplace_back(p);
				}
				// sort so we move the first box last
				std::sort(move_ordered.begin(), move_ordered.end());
				if (c == 'v') { 
					std::reverse(move_ordered.begin(), move_ordered.end());
				}
				for (auto box : move_ordered) {
					auto box_m= box + step;
					std::swap(w[box_m.r][box_m.c], w[box.r][box.c]);
					r = new_r;
				}

				return true;
			}
		}

		return false; // never hit
	}

	int gps(const Warehouse & w)
	{
		int score = 0;
		for (int ri = 0; ri < (int)w.size(); ri++) {
			for (int ci = 0; ci < (int)w[0].size(); ci++) {
				auto cell = w[ri][ci];
				if ((cell == BOX) || (cell == BOXL)) {
					score += 100*ri + ci;
				}
			}
		} 
		return score;
	}

	void printWarehouse(Robot r, Warehouse w)
	{
		for (int ri = 0; ri < (int)w.size(); ri++) {
			for (int ci = 0; ci < (int)w[0].size(); ci++) {
				if ((r.r ==ri) && (r.c ==ci)) {
					std::cout << "@";
				} else {
					auto cell = w[ri][ci];
					if (cell == WALL) {
						std::cout << "#";
					} else if (cell == SPACE) {
						std::cout << ".";
					} else if (cell == BOX) {
						std::cout << "O";
					} else if (cell == BOXL) {
						std::cout << "[";
					} else if (cell == BOXR) {
						std::cout << "]";
					}
				} 
			}
			std::cout << "\n";
		}
		std::cout << "\n";
		std::cout << "\n";

	}

	int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		const auto ps = AH::ParseLineGroups(is, '|');
		auto [w,r] = parseWarehouse(ps[0]);
		for (auto c :  ps[1]) {
			moveRobot(r, w, c);
		}
		int p1 = gps(w);

		auto [w2,r2] = parseWarehouse(ps[0], false);
		for (auto c :  ps[1]) {
			moveRobot(r2, w2, c);
		}
		int p2 = gps(w2);

		AH::PrintSoln(15, p1, p2);
		return 0;
	}
}

#endif
