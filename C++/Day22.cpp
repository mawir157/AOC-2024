#ifdef DAY22

#include "AH.h"

namespace Day22
{

	int64_t hashPattern(int64_t v0, int64_t v1, int64_t v2, int64_t v3)
	{
		int64_t hash = 
			20 * 20 * 20 * (v3 + 9) + 
			20 * 20 * (v2 + 9) + 
			20 * (v1 + 9) + 
			(v0 + 9);

		return hash;
	}
	
	std::pair<std::vector<int64_t>, std::map<int64_t, int64_t>>
	generateRandomNumbers(int seed, int items)
	{
		std::vector<int64_t> random_numbers;
		std::vector<int64_t> diffs;
		
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
			diffs.emplace_back((random_numbers[i+1] % 10) - (random_numbers[i] % 10));
		}

		std::map<int64_t, int64_t> bananaPrices;
		for (int i = 0; i < (int)random_numbers.size() - 5; i++) {
			int h = hashPattern(diffs[i], diffs[i+1], diffs[i+2], diffs[i+3]);
			if (bananaPrices.count(h) == 0) {
				bananaPrices[h] = random_numbers[i + 4] % 10;
			}
		}

		return std::make_pair(random_numbers, bananaPrices);
	}

	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);

		int64_t p1 = 0;
		std::map<int64_t, int64_t> prices;
		for (auto i : is) {
			int v = std::stoi(i);
			auto [seq, bps] = generateRandomNumbers(v, 2000);
			p1 += seq.back();

			for (auto [k, v] : bps) {
				prices[k] += v;
			}
		}

		auto p2 = std::max_element(prices.begin(), prices.end(),
		[](const std::pair<int, int>& p1, const std::pair<int, int>& p2) {
        	return p1.second < p2.second;}
		)->second;
 
		AH::PrintSoln(22, p1, p2);
		return 0;
	}
}

#endif