#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

void print(std::vector <int> const &x) {
  for(int i=0; i < x.size(); i++)
    Rcout << x.at(i) << ' ';
}

void print(std::unordered_map<int, std::vector<int>> const &map) {
  for (auto const el: map) {
    Rcout << "[" << el.first << "] ";
    print(el.second);
    Rcout << "\n";
  }
  Rcout << "\n";
}


// [[Rcpp::export]]
std::unordered_map<std::string, std::vector<int>> createBuckets(CharacterVector x) {
  std::unordered_map<std::string, std::vector<int>> map;

  std::string key;
  for (int i = 0; i < x.size(); i++) {
    key = as<std::string>(x[i]);
    map[key].push_back(i + 1);
  }

  return map;
}

void mergeGroups(std::unordered_map<int, std::vector<int>> &groups, std::vector<int> &node_group, const int from, const int to) {
  // exit if groups are the same
  int from_group = node_group[from];
  int to_group = node_group[to];
  if (from_group == to_group) return;

  // otherwise move nodes from to_group to-from_group
  std::vector<int> &from_nodes = groups[from_group];
  std::vector<int> &to_nodes = groups[to_group];

  // add to_nodes into from_nodes group
  from_nodes.insert(from_nodes.end(), to_nodes.begin(), to_nodes.end());

  // assign all to_nodes the from_group
  for (auto const &el: to_nodes) {
    node_group[el] = from_group;
  }

  // remove all nodes from to_group
  // note that since to_nodes is a reference this also erases it!
  groups.erase(to_group);
}

// [[Rcpp::export]]
List groupEdges(const IntegerVector from, const IntegerVector to) {
  // create unique set of nodes
  std::set<int> nodes_set;
  set_union(
    from.begin(), from.end(),
    to.begin(), to.end(),
    std::inserter(nodes_set, nodes_set.begin())
  );

  // copy set back to vector. maybe not the most elegant...
  std::vector<int> nodes(nodes_set.begin(), nodes_set.end());
  nodes_set.clear();

  // initialize each node to its own group
  // initialize each group to contain just one node
  int nodes_size = nodes.size();
  std::unordered_map<int, int> node_ids;
  std::vector<int> node_group(nodes_size);
  std::unordered_map<int, std::vector<int>> groups;

  for (int i = 0; i < nodes.size(); i++) {
    node_ids[nodes[i]] = i;
    node_group[i] = i;
    groups[i] = {i};
  //for (auto const el: nodes) {
    //node_group[el] = el;
    //groups[el] = {el};
  }

  // loop through edges, merging groups
  int len = from.size();
  for (int i = 0; i < len; i++) {
    mergeGroups(groups, node_group, node_ids[from[i]], node_ids[to[i]]);
  }

  // create output
  std::vector<int> group_ids(nodes.size());

  int i = 0;
  for(auto const node: nodes) {
    //node_ids[i] = node;
    group_ids[i] = node_group[node_ids[node]];
    i++;
  }

  return List::create(Named("group") = group_ids, Named("doc") = nodes);
}

/*** R

*/
