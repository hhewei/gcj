#include <iostream>
#include <string>
#include <set>

using namespace std;


char g[128][128];

set<char> dist(int i1, int i2, int j1, int j2) {
  set<char> d;
  for (auto i = i1; i < i2; ++i) {
    for (auto j = j1; j < j2; ++j) {
      if (g[i][j] != '?')
        d.insert(g[i][j]);
    }
  }
  return d;
}

void solve(int i1, int i2, int j1, int j2) {
  set<char> d = dist(i1, i2, j1, j2);
  if (d.size() == 1) {
    char x = *d.begin();
    for (auto i = i1; i < i2; ++i) {
      for (auto j = j1; j < j2; ++j) {
        g[i][j] = x;
      }
    }
    return;
  }

  for (int k = i1+1; k < i2; ++k) {
    auto a = dist(i1, k, j1, j2);
    auto b = dist(k, i2, j1, j2);
    if (a.size() > 0 && b.size() > 0) {
      solve(i1, k, j1, j2);
      solve(k, i2, j1, j2);
      return;
    }
  }

  for (int k = j1+1; k < j2; ++k) {
    auto a = dist(i1, i2, j1, k);
    auto b = dist(i1, i2, k, j2);
    if (a.size() > 0 && b.size() > 0) {
      solve(i1, i2, j1, k);
      solve(i1, i2, k, j2);
      return;
    }
  }
}

int main()
{
  uint nCase;
  cin >> nCase;
  for (auto iCase = 1; iCase <= nCase; ++iCase) {
    int r, c;
    cin >> r >> c;
    for (auto i = 0; i < r; ++i) {
      string line;
      cin >> line;
      for (auto j = 0; j < c; ++j) {
        g[i][j] = line[j];
      }
    }

    solve(0, r, 0, c);

    cout << "Case #" << iCase << ":" << endl;
    for (auto i = 0; i < r; ++i) {
      for (auto j = 0; j < c; ++j) {
        cout << g[i][j];
      }
      cout << endl;
    }
  }

  return 0;
}
