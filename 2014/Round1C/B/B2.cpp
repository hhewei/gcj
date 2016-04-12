#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <cassert>

using namespace std;

int main()
{
  int nCases;
  cin >> nCases;
  for (int iCase = 1; iCase <= nCases; ++iCase) {
	int n;
	cin >> n;
	vector<string> inputs;
	for (int i = 0; i < n; ++i) {
	  string s;
	  cin >> s;
	  inputs.push_back(s);
	  cerr << (i ? " " : "") << s;
	}
	cerr << endl;
	
	size_t idx[n];
	for (size_t i = 0; i < n; ++i) {
	  idx[i] = i;
	}

	size_t ans = 0;
	string s;

	do {
	  s.clear();
	  for (size_t i = 0; i < n; ++i) {
		s += inputs[idx[i]];
	  }
	  
	  auto uend = unique(s.begin(), s.end());
	  sort(s.begin(), uend);
	  if (unique(s.begin(), uend) == uend) {
		++ans;
	  }
	} while (next_permutation(idx, idx + n));

	cout << "Case #" << iCase << ": " << ans << endl;
  }

  return 0;
}

