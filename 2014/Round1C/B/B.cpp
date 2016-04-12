#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <cassert>

using namespace std;

struct UJ {
  map<char, char> m;

  UJ() {
	for (char c = 'a'; c <= 'z'; ++c) {
	  m[c] = c;
	}
  }

  void uj(char x, char y) {
	m[leader(x)] = leader(y);
  }

  char leader(char x) {
	while (x != m[x])
	  x = m[x];
	return x;
  }

  vector<set<char> > get(set<char> members) {
	map<char, set<char> > g;
	set<char>::iterator it = members.begin();
	for (; it != members.end(); ++it) {
	  char c = *it;
	  g[leader(c)].insert(c);
	}
	vector<set<char> > r;
	map<char, set<char> >::iterator iter;
	for (iter = g.begin(); iter != g.end(); ++iter) {
	  r.push_back(iter->second);
	}
	return r;
  }
};

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
	}

	bool conflict = false;

	map<char, int> w, r;
	map<char, int> cycle;
	map<char, int> in, out;
	set<char> members;
	UJ groups;
	for (int i = 0; i < n; ++i) {
	  string s = inputs[i];

	  int p = 0;
	  int q = s.length() - 1;
	  char hd = s[p];
	  char tl = s[q];
	  r[hd]++;
	  r[tl]++;

	  while (p < s.length() && s[p] == hd)
		p++;
	  while (q >= 0 && s[q] == tl)
		q--;

	  if (hd == tl) {
		if (p > q) {
		  cycle[hd]++;
		} else {
		  conflict = true;
		}
	  } else {
		in[tl]++;
		out[hd]++;
		members.insert(hd);
		members.insert(tl);
		groups.uj(hd, tl);
	  }

	  cerr << s << ": " << p << " " << q << " " << hd << " " << tl << endl;

	  for (int j = p; j <= q; ) {
		char sj = s[j];
		w[sj]++;
		while (j <= q && s[j] == sj)
		  ++j;
	  }
	}

	for (char c = 'a'; c <= 'z'; ++c) {
	  if (w[c] == 0)
		continue;

	  if ((w[c] == 1 && r[c] > 0) || w[c] > 1) {
		// cerr << "Conflict! " << c << " " << w[c] << " " << r[c] << endl;
		conflict = true;
		break;
	  }
	}

	if (conflict) {
	  cout << "Case #" << iCase << ": " << 0 << endl;
	  continue;
	}

	cerr << "members:";
	for (auto i = members.begin(); i != members.end(); ++i) {
	  cerr << " " << *i;
	}
	cerr << endl;

	vector<long long> fs;

	vector<set<char> > grps = groups.get(members);
	cerr << "#grps: " << grps.size() << endl;
	for (size_t i = 0; i < grps.size(); ++i) {
	  cerr << "  grp[" << i << "]: ";
	  set<char> g = grps[i];
	  for (auto it = g.begin(); it != g.end(); ++it) {
		cerr << *it << " ";
	  }
	  cerr << endl;
	}

	for (char c = 'a'; c <= 'z'; ++c) {
	  int x = cycle[c];
	  if (x) cerr << "cycle[" << c << "]: " << x << endl;
	  for (size_t f = 2; f <= x; ++f) {
		fs.push_back(f);
	  }
	}

	long long cs = 0;
	for (char c = 'a'; c <= 'z'; ++c) {
	  if (cycle[c] && members.find(c) == members.end()) {
		cs++;
	  }
	}
	cerr << "#free-cycle: " << cs << endl;
	for (size_t f = 2; f <= grps.size() + cs; ++f) {
	  fs.push_back(f);
	}

	for (size_t i = 0; i < grps.size(); ++i) {
	  bool nogo = false;
	  int minus1 = 0, plus1 = 0;
	  set<char>& g = grps[i];
	  for (auto it = g.begin(); it != g.end(); ++it) {
		char c = *it;

		if (in[c] == 1 && out[c] == 0) {
		  plus1++;
		  continue;
		}

		if (in[c] == 0 && out[c] == 1) {
		  minus1++;
		  continue;
		}

		if (in[c] == 1 && out[c] == 1) {
		  continue;
		}
	   
		nogo = true;
		break;
	  }

	  if (nogo) {
		fs.push_back(0);
		break;
	  }

	  if (plus1 == 1 && minus1 == 1) {
		continue;
	  }

	  fs.push_back(0);
	  break;
	}

	sort(fs.begin(), fs.end());

	long long ans = 1;
	for (size_t i = 0; i < fs.size(); ++i) {
	  cerr << fs[i] << " ";
	  ans = (ans * fs[i]) % 1000000007;
	}
	cerr << endl;
	cout << "Case #" << iCase << ": " << ans << endl;
  }

  return 0;
}

