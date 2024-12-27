#ifdef DAY25

#include "AH.h"

namespace Day25
{
	typedef std::vector<int> KeyLock;
	
	std::pair<std::vector<KeyLock>, std::vector<KeyLock>>
	parseKeysAndLock(const std::vector<std::string> & ss)
	{
		std::vector<KeyLock> keys;
		std::vector<KeyLock> locks;
		KeyLock timKey = {-1,-1,-1,-1,-1};
		bool isLock = true;
		int row = 0;
		for (auto s : ss) {
			if (s.empty()) {
				if (isLock) {
					locks.emplace_back(timKey);
				} else {
					keys.emplace_back(timKey);
				}
				timKey = {-1,-1,-1,-1,-1};
				isLock = true;
				row = 0;
			} else {
				for (int i = 0; i < (int)s.size(); i++) {
					if (s.at(i) == '#') {
						timKey[i]++;
					} else {
						if (row == 0) {
							isLock = false;
						}
					}
					row++;
				}
			}
		}

		if (isLock) {
			locks.emplace_back(timKey);
		} else {
			keys.emplace_back(timKey);
		}

		return std::make_pair(keys, locks);
	}

	int aPerfectFit(
		const std::vector<KeyLock> keys,
		const std::vector<KeyLock> locks)
	{
		int count = 0;
		for (int ki = 0; ki < (int)keys.size(); ki++) {
			for (int li = 0; li < (int)locks.size(); li++) {
				bool youCompleteMe = true;
				for (int pin = 0; pin < 5; pin++) {
					if (keys[ki][pin] + locks[li][pin] > 5) {
						youCompleteMe = false;
						break;
					}
				}
				if (youCompleteMe) {
					count++;
				}
			}
		}
		return count;
	}
	
	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		auto [keys, locks] = parseKeysAndLock(is);

		AH::PrintSoln(
			25, 
			aPerfectFit(keys, locks),
			"Goodbye. I love you."
		);
		return 0;
	}
}

#endif