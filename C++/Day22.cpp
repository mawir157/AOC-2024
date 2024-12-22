#ifdef DAY22

#include "AH.h"

namespace Day22
{

    std::vector<int64_t> generateRandomNumbers(int seed, int items)
    {
        std::vector<int64_t> random_numbers;
        
        int64_t secret = seed;
        random_numbers.emplace_back(secret);
        for (int i = 0; i < items; i++) {
            secret = secret ^ (secret << 6);
            secret &= 0xffffff;
            secret = secret ^ (secret >> 5);
            secret &= 0xffffff;
            secret = secret ^ (secret << 11);
            secret &= 0xffffff;

            random_numbers.emplace_back(secret);
        }

        return random_numbers;
    }

    std::set<std::vector<int64_t>> possiblePatterns()
    {
        std::set<std::vector<int64_t>> patterns;
        for (int64_t e0 = 0; e0 < 10; e0++) {
            for (int64_t e1 = 0; e1 < 10; e1++) {
                for (int64_t e2 = 0; e2 < 10; e2++) {
                    for (int64_t e3 = 0; e3 < 10; e3++) {
                        for (int64_t e4 = 0; e4 < 10; e4++) {
                            std::vector<int64_t> pattern{e1 - e0, e2 - e1, e3 - e2, e4 - e3};
                            patterns.insert(pattern);
                        }
                    }
                }
            }
        }

        return patterns;
    }

    int64_t buyBanana(
        const std::vector<std::vector<int64_t>> & seqs,
        const std::vector<std::vector<int64_t>> & diffs
    )
    {
        int64_t best_banana = 0;
        auto patterns = possiblePatterns();
        for (auto pattern : patterns) {
            
            int64_t banana_pattern = 0;
            int seq_idx=0;
            for (auto diff : diffs) {
                std::vector<int64_t>::iterator it;
                it = std::search(diff.begin(), diff.end(), pattern.data(), pattern.data()+4);
                if (it != diff.end()) {
                    int idx = 4 + (it-diff.begin());
                    banana_pattern += seqs[seq_idx][idx];
                }
                seq_idx++;
            }
            if (banana_pattern >  best_banana) {
                best_banana = banana_pattern;
                printf("[%ld, %ld, %ld, %ld] -> ", pattern[0], pattern[1], pattern[2], pattern[3]);
                std::cout << best_banana << "\n";
            }
            
        }

        return best_banana;
    }

	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);

        int64_t p1 = 0;
        std::vector<std::vector<int64_t>> seqs;
        std::vector<std::vector<int64_t>> diffs;
        for (auto i : is) {
            int v = std::stoi(i);
            auto seq = generateRandomNumbers(v, 2000);
            p1 += seq.back();

            for (auto & s : seq) {
                s %= 10;
            }
            std::vector<int64_t> diff;
            for (int ni = 1; ni < (int)seq.size(); ni++) {
                diff.emplace_back((seq[ni] % 10) - (seq[ni-1] % 10));
            }

            diffs.emplace_back(diff);
            seqs.emplace_back(seq);
        }

        auto p2 = buyBanana(seqs, diffs);
 
		AH::PrintSoln(12, p1, p2);
		return 0;
	}
}

#endif