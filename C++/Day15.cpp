// #ifdef DAY14

#include "AH.h"

namespace Day15
{
    enum CELL { SPACE, BOX, WALL, BOXL, BOXR };
    
    struct Pos {
        int r,c;

        Pos(int r, int c) : r(r), c(c) {};
        Pos operator+(Pos const& obj) { return Pos(r + obj.r, c + obj.c); }
        Pos operator-(Pos const& obj) { return Pos(r - obj.r, c - obj.c); }
        bool operator==(Pos const& obj) { return ((r == obj.r) && (c == obj.c)); }
        bool operator!=(Pos const& obj) { return ((r != obj.r) || (c != obj.c)); }
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
                    r.c = 2*ci;
                    if (part1) {
                        row.emplace_back(SPACE);
                    } else {
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
                std::vector<Pos> to_move; // these are the cells we are moving
                to_move.emplace_back(new_r);
                if (cell == BOXL) { // we are pushing the left side of the box
                    to_move.emplace_back(new_r + Pos(0,1)); // so add the right
                } else {
                    to_move.emplace_back(new_r - Pos(0,1)); // else add the left
                }
                // is there space to move the move the blocks to ?
                bool blocked = false;
                bool open = false;
                for (auto p : to_move) {
                    
                }
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
                if (cell == BOX) {
                    score += 100*ri + ci;
                }
            }
       } 
       return score;
    }

    void printWarehouse(Robot r, Warehouse w)
    {
        for (int ri = 0; ri < w.size(); ri++) {
            for (int ci = 0; ci < w[0].size(); ci++) {
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
        printWarehouse(r,w);
        for (auto c :  ps[1]) {
            moveRobot(r, w, c);
        }
        printWarehouse(r,w);
        int p1 = gps(w);

        auto [w2,r2] = parseWarehouse(ps[0], false);
        printWarehouse(r2,w2);
        for (auto c :  ps[1]) {
            moveRobot(r2, w2, c);
        }
        printWarehouse(r2,w2);
        int p2 = 0;


		AH::PrintSoln(15, p1, p2);
		return 0;
	}
}

// #endif
