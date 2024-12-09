// #ifdef DAY09

#include "AH.h"

namespace Day09
{
	struct File {
		int start;
		int length;
		int id;

		File(int start, int length, int id) :
		start{start}, length{length}, id{id}
		{}
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
			if (sz != 0) {
				if (writing) {
					files.emplace_back(pos, sz, id);
					id++;
				} else {
					gaps.emplace_back(pos, sz, -1);
				}
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
	
	// just append to files/gaps then reorder by start
	void defrag2(std::vector<File> & files, std::vector<File> & gaps)
	{
		int to_move = 0;
		for (auto f : files) {
			if (f.id > to_move) {
				to_move = f.id;
			}
		}

		while (to_move >= 0) {
			for (auto & f : files) {
				bool moved = false;
				if (f.id != to_move) { //cba to do this efficiently yet
					continue;
				}
				// this hole was made for me
				for (int gi = 0; gi < (int)gaps.size(); gi++) {
					auto gap = gaps[gi];
					auto gapW = gap.length;

					if (gap.start > f.start) {
						continue;
					}

					if (f.length == gapW) {
						gaps.emplace_back(f.start, f.length, -1);
						f.start = gap.start;
						gaps.erase (gaps.begin()+gi);
						moved = true;
						break;
					} else if (f.length <= gapW) {
						gaps.emplace_back(f.start, f.length, -1);
						f.start = gap.start;
						gaps[gi].length -= f.length;
						gaps[gi].start += f.length;
						moved = true;
						break;
					}
				}
				
				if (moved) {
					std::sort(files.begin(), files.end(), [](const File &a, const File &b)
						{
							return a.start < b.start;
						});
					std::sort(gaps.begin(), gaps.end(), [](const File &a, const File &b)
						{
							return a.start < b.start;
						});

					// combine gaps
					std::vector<File> new_gaps;
					int cur_start = -1;
					int cur_length =0;
					for (auto g : gaps) {
						if (cur_start == -1) {
							cur_start = g.start;
						}

						if ((cur_start + cur_length) == g.start) {
							cur_length += g.length;
						} else {
							new_gaps.emplace_back(cur_start, cur_length, -1);
							cur_start = g.start;
							cur_length = g.length;
						}
					}
					new_gaps.emplace_back(cur_start, cur_length, -1);
					gaps = new_gaps;
					break;
				} else {
					break;
				}
			}
			to_move--;
		}

		return;
	}
	
	int64_t checkSum2(std::vector<File> & fs)	{
		int64_t sum = 0;

		for (auto f : fs) {
			for (auto s = f.start; s < f.start+f.length; s++) {
				sum += s * f.id;
			}
		}

		return sum;
	}

	int64_t checkSum(std::vector<int> & fs)	{
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

	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		auto fs = parseInput(is[0]);
		defrag(fs);
		auto p1 = checkSum(fs);

		auto [files, gaps] = parseInput2(is[0]);
		defrag2(files, gaps);
		auto p2 = checkSum2(files);

		AH::PrintSoln(9, p1, p2);
		return 0;
	}
}
// #endif
