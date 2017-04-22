#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <set>

using namespace std;

uint req[128];
uint dp[128][128];

// bool count(uint amt, uint unit, uint& cnt) {
//   amt *= 10;
//   uint g = amt / (9*unit);
//   if (amt <= 11 * unit * g) {
//     cnt = g;
//     return true;
//   }
//   return false;
// }

// map<uint, uint> inter(map<uint, uint>& a, map<uint, uint>& b) {
//   map<uint, uint> r;
//   for (auto it = a.begin(); it != a.end(); ++it) {
//     auto k = it->first;
//     if (b.find(k) != b.end()) {
//       r[k] = min(it->second, b[k]);
//     }
//   }
//   return r;
// }

bool match(set<uint>& a, set<uint>& b) {
  vector<uint> inter;
  set_intersection(a.begin(), a.end(), b.begin(), b.end(), back_inserter(inter));
  return inter.size() > 0;
}

set<uint> div(uint amt, uint unit) {
  amt *= 10;
  set<uint> s;
  uint lower = amt / (11*unit);
  uint upper = amt / (9*unit);
  for (auto g = lower; g <= upper; ++g) {
    if ((9*unit*g <= amt) && (amt <= 11*unit*g))
      s.insert(g);
  }
  return s;
}

int main()
{
  uint nCase;
  cin >> nCase;
  for (auto iCase = 1; iCase <= nCase; ++iCase) {
    uint n, p;
    cin >> n >> p;
    for (auto i = 0; i < n; ++i) {
      cin >> req[i];
    }

    vector<vector<set<uint>>> all;
    for (auto i = 0; i < n; ++i) {
      vector<uint> amts;
      for (auto j = 0; j < p; ++j) {
        uint amt;
        cin >> amt;
        amts.push_back(amt);
      }
      sort(amts.begin(), amts.end());

      vector<set<uint>> pkgs;
      for (auto j = 0; j < p; ++j) {
        // cout << "amts[j]: " << amts[j] << " [ ";
        auto s = div(amts[j], req[i]);
        // for (auto it = s.begin(); it != s.end(); ++it) {
        //   cout << *it << " ";
        // }
        // cout << "]" << endl;
        pkgs.push_back(s);
      }
      all.push_back(pkgs);
    }

    uint ans = 0;
    if (n == 1) {
      for (auto i = 0; i < all[0].size(); ++i) {
        if (all[0][i].size() > 0)
          ++ans;
      }
    } else {
      for (auto i = 0; i < p; ++i) {
        for (auto j = 0; j < p; ++j) {
          if (i == 0 && j == 0) {
            dp[0][0] = match(all[0][0], all[1][0]) ? 1 : 0;
            continue;
          }
          if (i == 0 && j > 0) {
            uint x = match(all[0][i], all[1][j]) ? 1 : 0;
            dp[i][j] = max(dp[i][j-1], x);
            continue;
          }
          if (i > 0 && j == 0) {
            uint x = match(all[0][i], all[1][j]) ? 1 : 0;
            dp[i][j] = max(dp[i-1][j], x);
            continue;
          }
          uint cand = max(dp[i-1][j], dp[i][j-1]);
          if (match(all[0][i], all[1][j])) {
            cand = max(cand, dp[i-1][j-1]+1);
          }
          dp[i][j] = cand;
        }
      }
      // cout << "dp:" << endl;
      // for (auto i = 0; i < p; ++i) {
      //   for (auto j = 0; j < p; ++j) {
      //     cout << dp[i][j] << ' ';
      //   }
      //   cout << endl;
      // }
      ans = dp[p-1][p-1];
    }

    cout << "Case #" << iCase << ": " << ans << endl;
  }

  return 0;
}
