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
            // std::cout << "[" << m.A << "|" << m.B << "|" << m.C << "]" <<
            // ins << "," << opr << "(" << ptr << ")\n";

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

            // for (auto o : output) {
            //     std::cout << o << " ";
            // }
            // std::cout << "\n";
        }

        return output;
    }
    
    int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		auto [m, prg] = parseInput(is);	
        std::cout << m.A << "|" << m.B << "|" << m.C << "\n";
        for (auto i : prg) {
            std::cout << i << ",";
        }
        std::cout << "\n";

        auto p1 = runProgram(m,prg);
        for (auto t : p1) {
            std::cout << t << ",";
        }
        std::cout << "\n";

        // soln is of the order 8^prg.size()
        // 8 ^ 16 = 2^32
        for (int i = 0; i < 10000; i++) {
            m.A = i;m.B=0;m.C=0;
            auto p1 = runProgram(m,prg);
            printf("%d | %o: ",i,i);
            for (int i = p1.size() - 1; i >= 0; i--) {
                std::cout << p1[i] << " "; 
            }
            std::cout << "\n";
        }
		

		AH::PrintSoln(17, 0, 0);
		return 0;
	}

}

#endif