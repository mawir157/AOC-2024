#ifdef DAY09

#include "AH.h"

namespace Day09
{
	struct File {
		int start;
		int length;
		int id;
	};

	std::vector<int> parseInput(std::string s)
	{
		std::vector<int> fs;
		fs.reserve(200000);
		bool writing = true;
		int id = 0;

		for (auto c : s) {
			int idx = c - '0';
			if (writing) {
				fs.insert(fs.end(), idx, id);
				id++;
			} else {
				fs.insert(fs.end(), idx, -1);
			}
			writing = !writing;
		}

		return fs;
	}

	std::pair<std::vector<File>, std::vector<File>> parseInput2(std::string s)
	{
		std::vector<File> files;
		std::vector<File> gaps;
		
		bool writing = true;
		int id = 0;
		int pos = 0;
		for (auto c : s) {
			int sz = c - '0';
			if (writing) {
				files.emplace_back(pos, sz, id);
				id++;
			} else {
				gaps.emplace_back(pos, sz, -1);
			}
			pos += sz;
			writing = !writing;
		}

		return std::make_pair(files, gaps);
	}

	void defrag(std::vector<int> & fs)
	{
		auto it_l = std::find_if (fs.begin(), fs.end(), [](int x) { return x < 0;} );
		auto it_r = std::find_if (fs.rbegin(), fs.rend(), [](int x) { return x >= 0;});

		int counter = 0;
		while (it_l < it_r.base() - 1) {
			counter++;
			// std::swap can't be used w/ forward and reverse iterators!
			int temp = *it_l;
			*it_l = *it_r;
			*it_r = temp;

			it_l = std::find_if (fs.begin(), fs.end(), [](int x) { return x < 0;} );
			it_r = std::find_if (fs.rbegin(), fs.rend(), [](int x) { return x >= 0;});
		}

		return;
	}

	void defrag2(std::vector<File> & files, std::vector<File> & gaps)
	{
		return;
	}

	int64_t checkSum(std::vector<int> & fs)
	{
		int64_t checkSum = 0;
		for (int64_t i = 0; i < (int64_t)fs.size(); i++) {
			auto v = fs[i];
			if (v < 0) {
				break;
			}
			checkSum += i * v;
		}

		return checkSum;
	}

	int Run(const std::string& filename)
	{
		const auto grid = AH::ReadTextFile(filename);
		auto fs = parseInput(grid[0]);

		defrag(fs);

		AH::PrintSoln(9, checkSum(fs), 1);

		return 0;
	}

}

#endif
