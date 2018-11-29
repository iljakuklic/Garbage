#include <stdint.h>
#include <math.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

__attribute__((noinline))
uint64_t clz(uint64_t x) {
    return __builtin_clzll(x);
}

uint64_t isqrt_2iter(uint64_t x) {
    if (x < 9) {
        return (x > 0) + (x > 3);
    }
    uint64_t s = 32 - (clz(x) >> 1);
    uint64_t y = (uint64_t)1 << s;
    // y = (y + (x / y)) / 2;
    y = (y + (x >> s)) / 2;
    return y;
}

uint64_t isqrt_bybit(uint64_t x) {
    if (x < 9) {
        return (x > 0) + (x > 3);
    }
    uint64_t s = (63 - clz(x)) / 2;
    uint64_t y = (uint64_t)1 << s;
    uint64_t y2 = y << s;
    s--;
    do {
        // y2next = (y + 2^s)^2 = y^2 + 2*2^s + 2^(2s);
        uint64_t y2next = y2 + ((uint64_t)2 << s) + ((uint64_t)1 << (2 * s));
        if (y2next <= x) {
            y2 = y2next;
            y |= ((uint64_t)1 << s);
        }
    } while (s--);
    return y;
}

uint64_t isqrt_bybit2(uint64_t x) {
    if (x < 4) {
        return (x > 0);
    }
    uint64_t s = (63 - clz(x)) / 2;
    uint64_t y = (uint64_t)1 << s;
    uint64_t y2 = y << s;
    uint64_t b = y >> 1;
    
    while (b) {
        --s;
        // b = 2^s
        // y2next = (y + b)^2 = y^2 + 2yb + b^2
        // y2next = (y + 2^s)^2 = y^2 + 2y(2^s) + (2^s)^2
        // y2next = (y + 2^s)^2 = y^2 + y(2^(s+1)) + 2^s*2^s
        // y2next = (y + 2^s)^2 = (y + b)^2 = y^2 + y(2^(s+1)) + b*2^s
        uint64_t y2next = y2 + (y << (s + 1)) + (b << s);
        if (y2next <= x) {
            y2 = y2next;
            y |= b;
        }
        b >>= 1;
    };
    return y;
}

uint64_t isqrt_bybit3(uint64_t x) {
    if (x < 4) {
        return (x > 0);
    }
    uint64_t s = (63 - clz(x)) / 2;
    uint64_t y = (uint64_t)1 << s;
    uint64_t e = x - (y << s);
    uint64_t b = y >> 1;
    
    while (b) {
        --s;
        // b = 2^s
        // y2next = (y + b)^2 = y^2 + 2yb + b^2
        // y2next = (y + 2^s)^2 = y^2 + 2y(2^s) + (2^s)^2
        // y2next = (y + 2^s)^2 = y^2 + y(2^(s+1)) + 2^s*2^s
        // y2next = (y + 2^s)^2 = (y + b)^2 = y^2 + y(2^(s+1)) + b*2^s
        int64_t enext = e - (y << (s + 1)) - (b << s);
        if (enext >= 0) {
            e = enext;
            y |= b;
        }
        b >>= 1;
    };
    return y;
}

uint64_t isqrt_ref(uint64_t x) {
    return sqrt(x);
}

uint64_t absdiff(uint64_t a, uint64_t b) {
    return a > b ? a - b : b - a;
}

int main(void) {
    uint64_t maxerr = 0;
    for (uint64_t x = 1; x < 1000000; x++) {
        uint64_t y = isqrt_bybit3(x), yref = isqrt_ref(x);
        uint64_t err = absdiff(y, yref);
        if (err > maxerr) {
            printf("x = %10llu, y = %10llu, ref = %10llu, err = %10llu\n",
                   x, y, yref, err);
            maxerr = err;
        }
    }
}
