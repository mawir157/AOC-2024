#ifdef DAY17

#include "AH.h"

namespace Day17
{

	struct Machine {
        int64_t A;
        int64_t B;
        int64_t C;
    };

    typedef std::vector<int64_t> Program;

    std::pair<Machine, Program> parseInput(std::vector<std::string> ss)
    {
        Machine m;
        m.A = AH::stoi64(ss[0].substr(12));
        m.B = AH::stoi64(ss[1].substr(12));
        m.C = AH::stoi64(ss[2].substr(12));

        auto vs = ss[4].substr(9);
        auto ps = AH::Split(vs, ',');
        Program prg;
        for (auto p : ps) {
            prg.emplace_back(AH::stoi64(p));
        }

        return std::make_pair(m, prg);
    }

    int64_t combo(int64_t i, Machine m) {
        if ((0 <= i) && (i <= 3)) {
            return i;
        }
        if (i == 4) {
            return m.A;
        }
        if (i == 5) {
            return m.B;
        }
        if (i == 6) {
            return m.C;
        }
        return (int64_t)0;// never hit
    };

    std::vector<int64_t> runProgram(Machine & m, const Program p)
    {
        size_t ptr = 0;
        std::vector<int64_t> output;
        while (ptr < p.size()) {
            auto ins = p[ptr];
            auto opr = p[ptr+1];

            switch (ins)
            {
            case 0: //adv
            {
                auto A = m.A;
                auto c = combo(opr, m);
                A = A >> c;
                m.A = A;
                ptr += 2;
                break;
            }
            case 1: //bxl
            {
                auto B = m.B;
                B = B ^ opr;
                m.B = B;
                ptr += 2;
                break;
            }
            case 2: // bst
            {
                auto c = combo(opr, m);
                c %= 8;
                m.B = c;
                ptr += 2;
                break;
            } 
            case 3: // jnz
            {
                auto A = m.A;
                if (A != 0) {
                    ptr = opr;
                } else {
                    ptr += 2;
                }
                break;
            }
            case 4: //bxc
            {
                auto B = m.B;
                auto C = m.C;
                m.B = B ^ C;
                ptr += 2;
                break;
            }
            case 5: // out
            {
                auto c = combo(opr, m);
                c %= 8;
                output.emplace_back(c);
                ptr += 2;
                break;
            }
            case 6: //bdv
            {
                auto A = m.A;
                auto c = combo(opr, m);
                A = A >> c;
                m.B = A;
                ptr += 2;
                break;
            }
            case 7: //cdv
            {
                auto A = m.A;
                auto c = combo(opr, m);
                A = A >> c;
                m.C = A;
                ptr += 2;
                break;
            }
            default:
                break;
            }
        }

        return output;
    }

    int64_t vectorToInt(std::vector<int64_t> vs)
    {
        int64_t n = 0;
        for (auto v : vs) {
            n *= 8;
            n += v;
        }

        return n;
    }

    Program backTraceSoln(
        Program & v,
        int place,
        bool failed,
        Machine m,
        const Program & prg
    )
    {
        if ((place >= (int)v.size()) || (place < 0)) {
            return v;
        }
        
        uint64_t start = failed ? v[place] + 1 : v[place];
        if (failed) {
            for (int i = place + 1; i < (int)v.size(); i++) {
                v[i] = 0;
            }
        }

        for (uint64_t i = start; i < 8; i++) {
            v[place] = i;
            int64_t trial = vectorToInt(v);
            m.A = trial; m.B = 0; m.C = 0;
            auto test = runProgram(m, prg);
            // does this produce the write output ?
            if (test[v.size() - 1 - place] == prg[v.size() - 1 -place]) {
                // if so try to find the next place
                return backTraceSoln(v, place+1, false, m, prg);
            }
        }
        // we haven't found a valid soln so the previous value is wrong
        return backTraceSoln(v, place-1, true, m, prg);
    }
    
    int64_t Run(const std::string& filename)
	{ 
        const auto is = AH::ReadTextFile(filename);
		auto [m, prg] = parseInput(is);	

        auto v1 = runProgram(m,prg);
        std::string p1 = "";
        for (auto v : v1) {
            p1 += std::to_string(v);
            p1 += ",";
        }
        p1 = p1.substr(0, p1.size() -1);

        std::vector<int64_t> v(prg.size());
        
        auto q = backTraceSoln(v,0,false,m,prg);
        int64_t p2 = vectorToInt(q);

		AH::PrintSoln(17, p1, p2);
		return 0;
	}

}

#endif
