
#include <cstdint>
#include <array>
#include <vector>
//#include <iostream>

using std::uint16_t;
//using std::cout;

/*

struct foo {
	//static uint16_t constexpr bleh[9] = {1,2,3,4,5,6,7,8,9};
	static constexpr std::array<uint16_t, 9> bleh = {1,2,3,4,5,6,7,8,9};
};

namespace foo {
	namespace {
		constexpr int N = 9;
		const std::array<uint16_t, N> bleh = {1,2,3,4,5,6,7,8,9};
	}
};

bool bar(const uint16_t* blah) {
	return std::equal(foo::bleh.begin(), foo::bleh.end(), blah);
}

int dot(std::vector<int>& a, const std::vector<int> b) {
	for (int i = 0; i < a.size(); i++) {
		a[i] = b[i];
	}
	return a.size();
}
*/

/*
__attribute__((noinline))
void val(std::vector<int>&& vec) { cout << "vec&&\n"; }
__attribute__((noinline))
void val(std::vector<int>& vec) { cout << "vec&\n"; }
//void val(std::vector<int> vec) { cout << "vec\n"; }

int main() {
	std::vector<int> v = {0, 1, 2, 3};
	printf("before %d\n",    int(v.size()));
	std::vector<int> u = std::move(v);
	printf("after  %d %d\n", int(v.size()), int(u.size()));
	return u[0];
}
*/

using VecInt = std::vector<std::uint32_t>;

void add(VecInt& r, const VecInt& a, const VecInt& b) {
	for (int i = 0; i < int(a.size()); ++i) {
		r[i] = a[i] + b[i];
	}
}

void add(VecInt& r, const VecInt& a, VecInt&& b) {
	for (int i = 0; i < int(a.size()); ++i) {
		r[i] = a[i] + b[i];
	}
}

VecInt add(const VecInt& a, VecInt&& b) {
	VecInt r(a.size());
	for (int i = 0; i < int(a.size()); ++i) {
		r[i] = a[i] + b[i];
	}
	return r;
}
