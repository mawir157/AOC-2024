#ifdef DAY06

#include "AH.h"

namespace Day06
{

	typedef std::vector<std::string> Grid;
	enum Dir { NORTH, EAST, SOUTH, WEST }; 

	Dir rotate (const Dir d) 
	{
		if (d == NORTH) {
			return EAST;
		} else if (d == EAST) {
			return SOUTH;
		} else if (d == SOUTH) {
			return WEST;
		} else {
			return NORTH;
		}
	}

	struct Pos {
		int r, c;
		Dir d;

		bool operator<(const Pos& other) const
		{
			if (r != other.r) { return r < other.r ;}
			if (c != other.c) { return c < other.c ;}
			if (d != other.d) { return d < other.d ;}
			return false; 
		}

	};
	struct Guard {
		Pos loc;
		std::map<Pos, int> visited;
	};

	Guard findGuard(const Grid & grid)
	{
		Guard g;
		g.loc.d = NORTH;
		for (int r = 0; r < (int)grid.size(); r++) {
			for (int c = 0; c < (int)grid[r].size(); ++c) {
				if (grid[r][c] == '^') {
					g.loc.r = r;
					g.loc.c = c;
					g.visited.clear();
					return g;
				}
			}
		}
		return g; // never hit
	}

	bool path(Guard & g, const Grid & grid)
	{
		int r_max = grid.size();
		int c_max = grid[0].size();

		while (g.loc.r < r_max && g.loc.r >= 0 &&
			  g.loc.c < c_max && g.loc.c >= 0) {

			if (g.visited.count(g.loc)) { // we've been here before..
				return true;
			}

			g.visited[g.loc]++;
			
			// move one step a a time then refeactor later
			Pos next_loc = g.loc;
			switch (g.loc.d)
			{
			case NORTH:
				next_loc.r--;
				break;
			case EAST:
				next_loc.c++;
				break;
			case SOUTH:
				next_loc.r++;
				break;
			case WEST:
				next_loc.c--;
				break;
			}

			// leaving the grid
			if (next_loc.r >= r_max || next_loc.r < 0 ||
			  next_loc.c >= c_max || next_loc.c < 0) {
				return false; // not a closed loop
			}

			// what is at next_loc
			if ((grid[next_loc.r][next_loc.c] == '^') ||
				(grid[next_loc.r][next_loc.c] == '.')) { // empty space
				g.loc = next_loc;
			} else { // hit a wall
				g.loc.d = rotate(g.loc.d);
			}
		}
		return false; // never hit
	}

	std::map<Pos, int> findUniquePos(const std::map<Pos, int> visited)
	{
		std::map<Pos, int> unique;
		for (auto [k, v] : visited) {
			Pos p = k;
			p.d = NORTH;
			unique[p]++;
		}

		return unique;
	}

	int part2(const Grid & grid, const Guard guard, const std::map<Pos, int> pt) {
		int counter = 0;
		for (auto [p, _] : pt) {
			Grid new_grid = grid;
			Guard new_guard = guard;
			if (new_grid[p.r][p.c] == '.') {
				new_grid[p.r][p.c] = '#';
				if (path(new_guard, new_grid)) {
					counter++;
				}
			}
		}
		return counter;
	}

	int Run(const std::string& filename)
	{
		const auto grid = AH::ReadTextFile(filename);
		auto grd = findGuard(grid);
		path(grd, grid);
		auto path =  findUniquePos(grd.visited);
		grd = findGuard(grid);
		int p2 = part2(grid, grd, path);

		AH::PrintSoln(6, path.size(), p2);

		return 0;
	}

}

#endif
