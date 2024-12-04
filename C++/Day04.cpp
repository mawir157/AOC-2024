#ifdef DAY04

#include "AH.h"

namespace Day04
{
	typedef std::vector<std::string> Grid;
	
	int findWord(const Grid & g, const std::string target)
	{
		std::string revTarget = target;
		std::reverse(revTarget.begin(), revTarget.end());

		const size_t rows = g.size();
		const size_t cols = g[0].size();
		const size_t target_size = target.size();

		int counter = 0;
		// W->E & E->W
		for (size_t r = 0; r < rows; r++) {
			for (size_t c = 0; c < cols - target_size + 1; c++) {
				bool good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r][c + idx] != target.at(idx)) {
						good = false;
						break;
					}
				}
				if (good) {
					counter++;
				}
			
				good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r][c + idx] != revTarget.at(idx)) {
						good = false;
						break;
					}
				}
				if (good) {
					counter++;
				}
			 }
		}

		//N->S & S->N
		for (size_t r = 0; r < rows - target_size + 1; r++) {
			for (size_t c = 0; c < cols; c++) {
				bool good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r + idx][c] != target.at(idx)) {
						good = false;
						break;
					}
				}
				if (good) {
					counter++;
				}
			
				good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r + idx][c] != revTarget.at(idx)) {
						good = false;
						break;
					}
				}
				if (good) {
					counter++;
				}
			 }
		}

		//NW->SE & SE->NW
		for (size_t r = 0; r < rows - target_size + 1; r++) {
			for (size_t c = 0; c < cols - target_size + 1; c++) {
				bool good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r + idx][c + idx] != target.at(idx)) {
						good =false;
						break;
					}
				}
				if (good) {
					counter++;
				}
				
				good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r + idx][c + idx] != revTarget.at(idx)) {
						good = false;
						break;
					}
				}
				if (good) {
					counter++;
				}
			}
		}

		//NE->SW & SW->NE
		for (size_t r = 0; r < rows - target_size + 1; r++) {
			for (size_t c = target_size - 1; c < cols; c++) {
				bool good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r + idx][c - idx] != target.at(idx)) {
						good =false;
						break;
					}
				}
				if (good) {
					counter++;
				}
				
				good = true;
				for (size_t idx = 0; idx < target_size; idx++) {
					if (g[r + idx][c - idx] != revTarget.at(idx)) {
						good = false;
						break;
					}
				}
				if (good) {
					counter++;
				}
			}
		}

		return counter;
	}

	int findX(const Grid & g)
	{
		const size_t rows = g.size();
		const size_t cols = g[0].size();
		int counter = 0;

		for (size_t r = 1; r < rows - 1; r++) {
			for (size_t c = 1; c < cols - 1; c++) {
				if (g[r][c] != 'A') { continue; }

				if (
					(((g[r-1][c-1] == 'M') && (g[r+1][c+1] == 'S')) ||
					 ((g[r-1][c-1] == 'S') && (g[r+1][c+1] == 'M'))) &&
					(((g[r-1][c+1] == 'M') && (g[r+1][c-1] == 'S')) ||
					 ((g[r-1][c+1] == 'S') && (g[r+1][c-1] == 'M')))
				) {
					counter++;
				}
			}
		}

		return counter;
	}

	int Run(const std::string& filename)
	{
		auto g = AH::ReadTextFile(filename);
		auto p1 = findWord(g, "XMAS");
		auto p2 = findX(g);

		AH::PrintSoln(4, p1, p2);

		return 0;	
	}

}

#endif
