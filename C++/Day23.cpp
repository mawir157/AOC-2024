#ifdef DAY23

#include "AH.h"

namespace Day23
{

	typedef std::vector<std::vector<int>> Adj;
	typedef std::pair<Adj, std::map<std::string, int>> Graph;

	Graph buildGraph(std::vector<std::string> ss)
	{
		// get unique labels
		int labels = 0;
		std::map<std::string, int> map_labels;
		for (auto s : ss) {
			auto ps = AH::Split(s, '-');
			if (map_labels.count(ps[0]) == 0) {
				map_labels[ps[0]] = labels;
				labels++;
			}
			if (map_labels.count(ps[1]) == 0) {
				map_labels[ps[1]] = labels;
				labels++;
			}

		}

		// initialise adj matrix
		Adj adj(map_labels.size());
		for (auto & row : adj) {
			row.resize(map_labels.size());
		}
		// Now fill the adj matrix
		for (auto s : ss) {
			auto ps = AH::Split(s, '-');
			auto r = map_labels[ps[0]];
			auto c = map_labels[ps[1]];
			adj[r][c] = 1;
			adj[c][r] = 1;
		}

		return std::make_pair(adj, map_labels);
	}

	std::string find(const std::map<std::string, int> m, const int idx)
	{
		for (auto [k,v] : m) {
			if (v == idx) {
				return k;
			}
		}
		return "ERROR!";
	}

	std::vector<std::vector<std::string>>
	findTriangleSubgraphs(const Adj adj, const std::map<std::string, int> labels)
	{
		std::vector<std::vector<std::string>> subgraphs;

		for (int i0 = 0; i0 < (int)labels.size(); i0++) {
			for (int i1 = i0 + 1; i1 < (int)labels.size(); i1++) {
				if (adj[i0][i1] == 0) {
					continue;
				}
				for (int i2 = i1 + 1; i2 < (int)labels.size(); i2++) {
					if (adj[i1][i2] == 0) {
						continue;
					}
					if (adj[i2][i0] == 0) {
						continue;
					}
					std::vector<std::string> triangle;
					triangle.emplace_back(find(labels, i0));
					triangle.emplace_back(find(labels, i1));
					triangle.emplace_back(find(labels, i2));

					subgraphs.emplace_back(triangle);
				}
			}
		}

		return subgraphs;
	}


	std::set<int> nbrs(int idx, Adj adj)
	{
		std::set<int> ns;
		for (int n = 0; n < (int)adj.size(); n++) {
			if (adj[idx][n] == 1) {
				ns.insert(n);
			}
		}

		return ns;
	}

	// Stolen from Wikipedia 'cause I'm a scrub who knows nothing about
	// computational graph theory
	void BronKerbosch(
		std::set<int> R,
		std::set<int> P,
		std::set<int> X,
		const Adj & adj,
		std::vector<std::set<int>> & results
	)
	{
		if (P.empty() && X.empty()) {
			results.emplace_back(R);
			return;
		}

		std::set<int> P_copy = P;

		for (auto v : P_copy) {
			std::set RR = R;
			RR.insert(v);
			auto NV = nbrs(v, adj);
			std::set<int> PP;
			set_intersection(P.begin(), P.end(), NV.begin(), NV.end(),
			                 std::inserter(PP, PP.begin()));
			std::set<int> XX;
			set_intersection(X.begin(), X.end(), NV.begin(), NV.end(),
			                 std::inserter(XX, XX.begin()));

			BronKerbosch(RR, PP, XX, adj, results);

			P.erase(v);
			X.insert(v);
		}

		return;
	}

	std::string part2(const Adj adj, const std::map<std::string, int> labels)
	{
		std::set<int> R;
		std::set<int> P;
		std::set<int> Q;
		for (int i = 0; i < (int)labels.size(); i++) {
			P.insert(i);
		}

		std::vector<std::set<int>> results;
		BronKerbosch(R, P, Q, adj, results);

		size_t max_result = 0;
		std::set<int> max_graph;

		for (auto r : results) {
			if (r.size() > max_result) {
				max_result = r.size();
				max_graph = r;
			}
		}

		std::vector<std::string> vs;
		for (auto r : max_graph) {
			vs.emplace_back(find(labels, r));
		}
		std::sort(vs.begin(), vs.end());

		std::string p2 = "";
		for (auto s : vs) {
			p2 += s;
			p2 += ",";
		}

		return p2;
	}

	int Run(const std::string& filename)	{
		const auto is = AH::ReadTextFile(filename);
		auto [adj, labels] = buildGraph(is);

		int p1 = 0;
		auto triangles = findTriangleSubgraphs(adj, labels);
		for (auto t : triangles) {
			for (auto node : t) {
				if (node.at(0) == 't') {
					p1++;
					break;
				}
			}
		}

		auto p2 = part2(adj, labels);

		AH::PrintSoln(23, p1, p2);
		return 0;
	}
}

#endif