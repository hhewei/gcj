#include <iostream>

using namespace std;

void flip(char& c) {
  if (c == '+')
    c = '-';
  else
    c = '+';
}

bool solve(string s, uint k, uint& ans) {
  ans = 0;
  for (auto i = 0; i + k <= s.size(); ++i) {
    if (s[i] != '+') {
      for (auto j = 0; j < k; ++j) {
        flip(s[i+j]);
      }
      ++ans;
    }
  }

  for (auto i = 0; i < s.size(); ++i) {
    if (s[i] != '+')
      return false;
  }
  return true;
}

int main()
{
  uint nCase;
  cin >> nCase;
  for (auto iCase = 1; iCase <= nCase; ++iCase) {
    string s;
    uint k;
    cin >> s >> k;

    cout << "Case #" << iCase << ": ";
    uint ans;
    if (solve(s, k, ans)) {
      cout << ans;
    } else {
      cout << "IMPOSSIBLE";
    }
    cout << endl;
  }

  return 0;
}
