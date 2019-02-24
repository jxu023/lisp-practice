#include <iostream>
#include <set>

using namespace std;

int main()
{
  // read in nums until eof
  set<long> prev_freqs;
  prev_freqs.insert(0);
  long freq;
  long num;

  while (cin.good())
  {
    cin >> num;
    freq += num;
    cout << "num: " << num << " freq: " << freq << '\n';

    if (prev_freqs.find(freq) != prev_freqs.end()) {
      cout << "found a repeated frequency\n";
      break;
    }
  }
}
