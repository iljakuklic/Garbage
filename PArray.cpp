#include <cstdlib>
#include <vector>
#include <memory>
#include <cstdio>

template <typename T>
class PArray {
public:
    // TYPE ALIASES
    using value_type = T;
    using size_type = size_t;

    // FUNDAMENTAL OPERATIONS

    // Create a new array.
    PArray(std::vector<T>&& initial);

    // Get an array element value.
    const T& get(size_t i);
    
    // Write an array element, returning the updated array.
    // This keeps the old array obseravtionally unmodified.
    PArray put(size_t i, const T& val);

    // Query array size.
    size_t size() { return impl->data.size(); }

private:
    // IMPLEMENTATION TYPES
    
    // History entry is a pair of index and value.
    using HistoryEntry = std::pair<size_t, T>;

    struct Internal {
        std::vector<T> data;
        std::vector<HistoryEntry> history;
        Internal(std::vector<T>&& d) : data(d) {}
    };

    using InternalPtr = std::shared_ptr<Internal>;

private:
    // PRIVATE DATA
    
    InternalPtr impl;
    size_t version;
};

// New persistent array.
template <typename T>
PArray<T>::PArray(std::vector<T>&& initial)
    : impl(std::make_shared<Internal>(std::move(initial))),
      version(0) {}

// Get element.
template <typename T>
const T& PArray<T>::get(size_t i) {
    typename std::vector<HistoryEntry>::iterator
        hist_begin = impl->history.begin() + version,
        hist_end = impl->history.end();

    // Consult the history first for requested value.
    for (auto hi = hist_begin; hi != hist_end; ++hi) {
        if (hi->first == i) {
            return hi->second;
        }
    }

    // Value not in history, fetch the most up-to-date version.
    return impl->data[i];
}

// Set element.
template <typename T>
PArray<T> PArray<T>::put(size_t i, const T& val) {
    // Create a new array to return.
    PArray<T> new_ary = *this;

    if (version != impl->history.size()) {
        // Copy the array if not at last version.
        std::vector<T> new_data(impl->data.size());
        for (size_t i = 0; i < impl->data.size(); i++) {
            new_data[i] = get(i);
        }
        new_ary = PArray<T>(std::move(new_data));
    } else if (impl->history.size() >= impl->data.size()) {
        // Copy the array if history too large.
        new_ary = PArray<T>(std::vector<T>(impl->data));
    } else {
        // Otherwise extend the history with the old value.
        impl->history.push_back(std::make_pair(i, impl->data[i]));
        new_ary.version++;
    }

    // Finally, update the value.
    new_ary.impl->data[i] = val;

    return new_ary;
}

void dump(PArray<int>& a) {
    for (size_t i = 0; i < a.size(); i++) {
        printf("%d ", a.get(i));
    }
    printf("\n");
}

int main() {
    std::vector<int> init = { 5, 7, 8, 9, 4 };
    PArray<int> a1(std::move(init));
    PArray<int> a2 = a1.put(2, 5);
    PArray<int> a3 = a2.put(3, 1);
    for (int i = 0; i < 10; i++) {
        a3 = a3.put(4, i);
    }
    PArray<int> a4 = a2.put(0, 0);
    dump(a1);
    dump(a2);
    dump(a3);
    dump(a4);
}

