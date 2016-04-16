#include <iostream>
#include <map>
#include <set>
#include <list>

using namespace std;

void dfs(map<int, set<int> >& g, int i, set<int>& visited)
{
  if (visited.find(i) != visited.end())
    return;
  visited.insert(i);

  for (set<int>::const_iterator it = g[i].begin(); it != g[i].end(); ++it) {
    dfs(g, *it, visited);
  }
}

void cycle(map<int, int>& g, int i, list<int>& path, set<int>& visited)
{
  path.push_front(i);
  auto it = visited.find(i);
  if (it != visited.end())
    return;
  visited.insert(i);
  
  return cycle(g, g[i], path, visited);
}

void cycle(map<int, int>& g, int i, list<int>& path)
{
  set<int> v;
  return cycle(g, i, path, v);
}

int recLongest(map<int, int>& g, int cur, int tgt, int avoid, map<int, int>& dp)
{
  if (dp.find(cur) != dp.end())
    return dp[cur];

  if (cur == tgt)
    return 1;

  if (cur == avoid)
    return 0;

  int res = recLongest(g, g[cur], tgt, avoid, dp);
  if (res) ++res;
  dp[cur] = res;
  return res;
}

int longest(map<int, int>& g, set<int> ns, int tgt, int avoid)
{
  map<int, int> dp;
  int res = 0;
  for (auto it = ns.begin(); it != ns.end(); ++it) {
    res = max(res, recLongest(g, *it, tgt, avoid, dp));
  }
  return res;
}

int main()
{
  int t;
  cin >> t;
  for (int i = 1; i <= t; ++i) {
    int n;
    cin >> n;

    map<int, set<int> > g;
    map<int, int> dg;
    
    for (int j = 1; j <= n; ++j) {
      int bff;
      cin >> bff;
      g[j].insert(bff);
      g[bff].insert(j);
      dg[j] = bff;
    }

    int open = 0;
    int closed = 0;
    set<int> resolved;
    for (int j = 1; j <= n; ++j) {
      if (resolved.find(j) != resolved.end())
	continue;
      
      set<int> v;
      dfs(g, j, v);
      for (set<int>::const_iterator it = v.begin(); it != v.end(); ++it) {
	resolved.insert(*it);
      }

      list<int> path;
      cycle(dg, *v.begin(), path);
       auto it = path.begin();
      int tgt = *it++;
      int cnt = 1;
      while (*it++ != tgt)
	++cnt;

      if (cnt == 2) {
	int other = *++path.begin();
	int a = longest(dg, v, tgt, other);
	int b = longest(dg, v, other, tgt);
	open += a + b;
      } else {
	closed = cnt > closed ? cnt : closed;
      }
    }

    int res = max(open, closed);
    cout << "Case #" << i << ": " << res << endl;
  }
  
  return 0;   
}
