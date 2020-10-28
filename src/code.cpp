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

void assertSequential(std::set<int> x) {
  // check that set is  sequential by comparing size to max value
  int x_min = *x.begin();
  if (x_min != 0) {
    Rcpp::stop("node ids must start from 0");
  }

  int x_max = *x.rbegin();
  int x_size = x.size();

  if (x_max != (x_size - 1)) {
    Rcpp::stop("node ids must be in sequential order");
  }
}

// [[Rcpp::export]]
List groupEdges(const IntegerVector from, const IntegerVector to) {
  // create unique set of nodes
  std::set<int> nodes;
  set_union(
    from.begin(), from.end(),
    to.begin(), to.end(),
    std::inserter(nodes, nodes.begin())
  );

  // check that nodes are sequential
  assertSequential(nodes);

  // initialize each node to its own group
  // initialize each group to contain just one node
  int nodes_size = nodes.size();
  std::vector<int> node_group(nodes_size);
  std::unordered_map<int, std::vector<int>> groups;

  //for (int i = 0; i < nodes.size(); i++) {
  for (auto const el: nodes) {
    node_group[el] = el;
    groups[el] = {el};
  }

  // loop through edges, merging groups
  int len = from.size();
  for (int i = 0; i < len; i++) {
    mergeGroups(groups, node_group, from[i], to[i]);
  }

  // create output
  std::vector<int> node_ids(nodes.size());
  std::vector<int> group_ids(nodes.size());

  int i = 0;
  for(auto const node: nodes) {
    node_ids[i] = node;
    group_ids[i] = node_group[node];
    i++;
  }

  return List::create(Named("node") = node_ids , Named("group") = group_ids);
}

/*** R
timesTwo(42)
*/
