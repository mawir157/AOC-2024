#ifdef DAY13

#include "AH.h"

namespace Day13
{

	struct Game
	{
		int64_t ax, ay, bx, by, px, py;

		int64_t minSolve(const bool part2);
	};

	int64_t Game::minSolve(const bool part2)
	{
		if (part2) {
			px += 10000000000000;
			py += 10000000000000;
		}
			
		auto det = by * ax - bx * ay;
		if (det == 0) { // the vectors are colinear?
			return 0; // probaby correct - fix later if this is a probelem
		}
		auto b = py * ax - px * ay;
		auto a = by * px - bx * py;

		int64_t score = 0;
		if (a % det == 0) {
			score += 3 * (a / det);
		} else {
			return 0;
		}

		if (b % det == 0) {
			score += 1 * (b / det);
		} else {
			return 0;
		}
		
		return score;

	}

	
	std::vector<Game> parseInput(std::vector<std::string> ss, char c)
	{
		std::vector<Game> gs;

		for (auto s: ss) {
			Game g;
			auto ps = AH::Split(s, c);
			for (auto p : ps) {
			}
			size_t from = 0;
			size_t to = 0;
			std::string p = "";
			std::string number = "";
			// Button A
			p = ps[0];
			from = p.find("+", from);
			to = p.find(",", to);
			number = p.substr (from+1,to);
			g.ax = std::stoi(number);
			from = p.find("+", from+1);
			number = p.substr(from+1);
			g.ay = std::stoi(number);

			// Button B
			p = ps[1];
			from = 0;
			to = 0;
			from = p.find("+", from);
			to = p.find(",", to);
			number = p.substr (from+1,to);
			g.bx = std::stoi(number);
			from = p.find("+", from+1);
			number = p.substr (from+1);
			g.by = std::stoi(number);

			// Prize
			p = ps[2];
			from = 0;
			to = 0;
			from = p.find("=", from);
			to = p.find(",", to);
			number = p.substr(from+1,to);
			g.px = std::stoi(number);
			from = p.find("=", from+1);
			number = p.substr(from+1);
			g.py = std::stoi(number);

			gs.push_back(g);
		}

		return gs;
	} 

	int64_t Run(const std::string& filename)
	{
		const auto is = AH::ReadTextFile(filename);
		const auto lgs = AH::ParseLineGroups(is, '#');
		auto gs = parseInput(lgs, '#');
		int64_t p1 = 0;
		int64_t p2 = 0;
		for (auto g : gs) {
			p1 += g.minSolve(false);
			p2 += g.minSolve(true);
		}

		AH::PrintSoln(13, p1, p2);
		return 0;
	}
}

#endif
