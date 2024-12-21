#ifdef DAY21

#include "AH.h"

namespace Day21
{
    struct Pos {
		int r,c;

		Pos(int r, int c) : r(r), c(c) {};
		Pos() : r(0), c(0) {};
		Pos operator+(Pos const& obj) const { return Pos(r + obj.r, c + obj.c); }
		Pos operator-(Pos const& obj) { return Pos(r - obj.r, c - obj.c); }
		bool operator==(Pos const& obj) { return ((r == obj.r) && (c == obj.c)); }
		bool operator!=(Pos const& obj) { return ((r != obj.r) || (c != obj.c)); }
		bool operator<(Pos const& obj) const {
			if (r != obj.r) 
				return r < obj.r;

			return c < obj.c;
		}
	};
    
    typedef std::map<char, Pos> lut;
    lut g_nPad_moves;

    lut initialise_NPad()
    {
        lut moves;
        moves['0'] = Pos(0, 1);
        moves['A'] = Pos(0, 2);
        moves['1'] = Pos(1, 0);
        moves['2'] = Pos(1, 1);
        moves['3'] = Pos(1, 2);
        moves['4'] = Pos(2, 0);
        moves['5'] = Pos(2, 1);
        moves['6'] = Pos(2, 2);
        moves['7'] = Pos(3, 0);
        moves['8'] = Pos(3, 1);
        moves['9'] = Pos(3, 2);
      
        moves['^'] = Pos(0, 1); // this might be a problem!
        moves['<'] = Pos(-1, 0);
        moves['v'] = Pos(-1, 1);
        moves['>'] = Pos(-1, 2);

        return moves;
    }

    
    std::string nPadTo2dPad(const std::string nPad, const lut moves)
    {
        char cur = 'A'; // always start at A
        std::string instruction = "";
        Pos p_cur = moves.at(cur);
        for (auto next : nPad) {
            Pos p_next = moves.at(next);

            const auto diff = p_next - p_cur;
            // can we do rows (cols) first without going through (0,0)?
            auto row_first = (p_cur + Pos(diff.r, 0)) != Pos(0,0);
            auto col_first = (p_cur + Pos(0, diff.c)) != Pos(0,0);
            // logic move +ve direction last unless overridden by row/col_first
            if (!row_first) { // we must do cols first
                for (int c = 0; c < abs(diff.c); c++) {
                    instruction += (diff.c > 0) ? ">" : "<";
                }
                for (int r = 0; r < abs(diff.r); r++) {
                    instruction += (diff.r > 0) ? "^" : "v";
                }
            } else if (!col_first) { // we must do rows first
                for (int r = 0; r < abs(diff.r); r++) {
                    instruction += (diff.r > 0) ? "^" : "v";
                }
                for (int c = 0; c < abs(diff.c); c++) {
                    instruction += (diff.c > 0) ? ">" : "<";
                }
            } else {
                if (diff.r > 0) { // do rows last so we end up near the A
                    for (int c = 0; c < abs(diff.c); c++) {
                        instruction += (diff.c > 0) ? ">" : "<";
                    }
                    for (int r = 0; r < abs(diff.r); r++) {
                        instruction += (diff.r > 0) ? "^" : "v";
                    }
                } else {
                    for (int r = 0; r < abs(diff.r); r++) {
                        instruction += (diff.r > 0) ? "^" : "v";
                    }
                    for (int c = 0; c < abs(diff.c); c++) {
                        instruction += (diff.c > 0) ? ">" : "<";
                    }
                }
            }

            instruction += "A";
            cur = next;
            p_cur = moves.at(cur);
        }

        return instruction;
    }

    int64_t score(std::string s0, const lut moves, const int robots)
    {
        int64_t i = std::stoi(s0.substr(0, s0.size()-1));
        
        auto robot_input = nPadTo2dPad(s0, moves);

        for (int i = 0; i < robots; i++) {
            robot_input = nPadTo2dPad(robot_input, moves);
        }

        std::cout << robot_input.size() << " * " << i << " = " << i * robot_input.size() <<"\n";
        return i * robot_input.size();    
    }
    
    int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
        const auto nPad = initialise_NPad();
        int64_t p1 = 0;
        for (auto i : is)  {
            const auto s = score(i, nPad, 2);
            p1 += s;
        }
        int64_t p2 = 0;
        // Brute force doesn't work :(
        // for (auto i : is)  {
        //     const auto s = score(i, nPad, 25);
        //     p2 += s;
        // }

		AH::PrintSoln(21, p1, p2);
		return 0;
	}
}

#endif