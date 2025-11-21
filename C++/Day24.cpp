#ifdef DAY24

#include "AH.h"

namespace Day24
{
	
	struct Rule
	{
		std::string lhs;
		std::string rhs;
		std::string out;
		std::string mode;
		bool applied = false;
	};

	struct Node
	{
		bool state;
		bool active;
	};
	
	std::map<std::string, Node> parseNodes(const std::vector<std::string> & ss)
	{
		std::map<std::string, Node> nodes;
		for (auto s : ss) {
			auto ps = AH::SplitOnString(s, ": ");
			Node node{.state = (ps[1] != "0"), .active = true};
			nodes[ps[0]] = node;
		}

		return nodes;
	}

	std::vector<Rule> parseRules(
		const std::vector<std::string> & ss,
		std::map<std::string, Node> & nodes
	)
	{
		std::vector<Rule> rules;
		for (auto s : ss) {
			auto ps = AH::SplitOnString(s, " ");
			Rule r{
				.lhs=ps[0],
				.rhs=ps[2],
				.out=ps[4],
				.mode=ps[1]
			};
			rules.emplace_back(r);
			if (nodes.count(ps[0]) == 0) {
				nodes[ps[0]] = Node{.state = false, .active = false};
			}
			if (nodes.count(ps[2]) == 0) {
				nodes[ps[2]] = Node{.state = false, .active = false};
			}
			if (nodes.count(ps[4]) == 0) {
				nodes[ps[4]] = Node{.state = false, .active = false};
			}
		}

		return rules;
	}

	int64_t applyRules(
		std::map<std::string, Node> nodes,
		std::vector<Rule> rules
	)
	{
		bool zs_done = false;

		while (!zs_done) {
			for (auto & r : rules) {
				if (r.applied ) { // we have applied this rule
					continue;
				}
				// if either of the inputs are inactive - wait
				if ((!nodes[r.lhs].active) || (!nodes[r.rhs].active)) {
					continue;
				}

				// we have active inputs so apply the rule
				nodes[r.out].active = true;
				if (r.mode == "AND") {
					nodes[r.out].state = nodes[r.lhs].state & nodes[r.rhs].state;
				} else if (r.mode == "OR") {
					nodes[r.out].state = nodes[r.lhs].state | nodes[r.rhs].state;
				} else if (r.mode == "XOR") {
					nodes[r.out].state = nodes[r.lhs].state ^ nodes[r.rhs].state;
				}
				// mark the rule as used
				r.applied = true;
			}

			zs_done = true;
			for (auto [k, v] : nodes) {
				if (k.at(0) == 'z') {
					zs_done &= v.active;
				}

				if (!zs_done) {
					break;
				}
			}
		}

		int64_t score = 0;
		std::vector<std::pair<std::string, bool>> pairs;
		for (auto [k, v] : nodes) {
			if (k.at(0) == 'z') {
				pairs.emplace_back(k, v.state);
			}
		}
		std::sort(pairs.begin(), pairs.end(), [](auto &left, auto &right) {
			return left.first > right.first;
		});

		for (auto [k, v]: pairs) {
			score *= 2;
			score += v ? 1 : 0;
		}

		return score;
	}
	
	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		bool nodes_rules = true;
		std::vector<std::string> ns;
		std::vector<std::string> rs;
		for (auto s :  is) {
			if (s.empty()) {
				nodes_rules = false;
				continue;
			}

			if (nodes_rules) {
				ns.emplace_back(s);
			} else {
				rs.emplace_back(s);
			}
		}
		auto nodes = parseNodes(ns);
		auto rules = parseRules(rs, nodes);
		int64_t p1 = applyRules(nodes, rules);


		// calculated by hand in a spreadsheet. Not sure if there is a simple
		// way to do this programmatically.
		std::string p2 = "bfq,bng,fjp,hkh,hmt,z18,z27,z31";
 
		AH::PrintSoln(24, p1, p2);
		return 0;
	}
}

#endif