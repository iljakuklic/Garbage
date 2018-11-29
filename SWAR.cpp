#include <initializer_list>
#include <cstdint>
#include <cstdio>

constexpr uint64_t GenMask(uint64_t mask, size_t BitWidth) {
	uint64_t old_mask = 0u;
	while (BitWidth < 64) {
		old_mask = mask;
		mask |= mask << BitWidth;
		BitWidth *= 2;
	}
	return mask;
}

template <size_t Bits>
class IntArray {
public:
	static constexpr size_t BitWidth = Bits;
	static constexpr size_t ArraySize = 64 / BitWidth;
	static constexpr uint64_t SignMask = GenMask(1ull << (BitWidth - 1), BitWidth);
	static constexpr uint64_t DataMask = ~SignMask;
	static constexpr uint64_t AllOnes = GenMask(1ull, BitWidth);
	static constexpr uint64_t OneMask = (1ull << BitWidth) - 1;

	IntArray(unsigned int init = 0) : data(init * AllOnes) {}

	IntArray(std::initializer_list<unsigned int> init) {
		int i = 0;
		for (unsigned int x : init) {
			// assert(x <= OneMask);
			data |= uint64_t(x) << i;
			i += BitWidth;
		}
	}

	unsigned int operator[](int i) const {
		return (data >> (i * BitWidth)) & OneMask;
	}

	IntArray operator+(IntArray y) const {
		IntArray r;
		r.data = (data & DataMask) + (y.data & DataMask);
		r.data ^= (data ^ y.data) & SignMask;
		return r;
	}

	constexpr size_t size() const { return ArraySize; }
	uint64_t get() const { return data; }
private:
	uint64_t data;
};

static_assert(IntArray<5>::ArraySize == 12, "");
static_assert(IntArray<4>::SignMask == 0x8888888888888888ull, "");
static_assert(IntArray<4>::AllOnes  == 0x1111111111111111ull, "");
static_assert(IntArray<6>::AllOnes  == 0x1041041041041041ull, "");

#define print(fmt, x) printf(#x " = %" #fmt "\n", x)
int main() {
	IntArray<11> a = {2,5,12,77};
	print(lx, a.get());
	a = a + a;
	print(u, a[0]);
	print(u, a[1]);
	print(u, a[2]);
	print(u, a[3]);
}
