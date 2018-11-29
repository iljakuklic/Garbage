#include <cstdlib>
#include <vector>
#include <memory>
#include <iostream>
#include <cassert>

#ifndef TRACE
#define TRACE(x) do { x; } while (false)
#endif

template <typename T>
class PArray {
public:
    // TYPE ALIASES
    using value_type = T;
    using size_type = size_t;
    using storage_t = std::vector<value_type>;
    using iterator = typename storage_t::iterator;

    // FUNDAMENTAL OPERATIONS

    // Create a new array.
    PArray(storage_t&& initial);
    PArray(std::initializer_list<T> init);
	~PArray();

    // Get an array element value.
    T get(size_t i) const { return (*data)[i]; }

    // Write an array element, returning the updated array.
    // This keeps the old array obseravtionally unmodified.
    static PArray put(const PArray& ary, size_t i, T val);
    static PArray put(PArray&& ary, size_t i, T val);

    // CONVENIENCE FEATURES

    T operator[](size_t i) const { return get(i); }
    size_t size() const { return data->size(); }
    iterator begin() const { return data->begin(); }
    iterator end() const { return data->end(); }
    void dump() const;

private:
    // IMPLEMENTATION DETAILS
    std::shared_ptr<storage_t> data;
};

template <typename T>
PArray<T>::PArray(storage_t&& initial)
        : data(std::make_shared<storage_t>(std::move(initial))) {
    TRACE(std::cout << "New array from storage at " << data->data() << "\n");
}

template <typename T>
PArray<T>::~PArray() {
	if (data.unique()) {
		TRACE(std::cout << "Deleting array " << data->data() << "\n");
	}
}

template <typename T>
PArray<T>::PArray(std::initializer_list<T> init)
        : data(std::make_shared<storage_t>(init)) {
    TRACE(std::cout << "New array from init list at " << data->data() << "\n");
}

template <typename T>
PArray<T> PArray<T>::put(PArray&& ary, size_t i, T val) {
	TRACE(std::cout << "RValue put into " << ary.data->data() << "\n");
    if (ary.data.unique()) {
        // We're the only reference, can just update in place.
        TRACE(std::cout << "Unique reference at " << ary.data->data() << "\n");
        (*ary.data)[i] = val;
        return ary;
    }

    if ((*ary.data)[i] == val) {
        // Writing the same element that's already there, return array unchanged.
        TRACE(std::cout << "Equal element at " << ary.data->data() << "\n");
        return ary;
    }
    
    // Otherwise, copy the data, set element and return the new array.
    PArray new_ary(storage_t(*ary.data));
    TRACE(std::cout << "Copying " << ary.data->data() << " -> "
                                  << new_ary.data->data() << "\n");
    (*new_ary.data)[i] = val;
    return new_ary;
}

template <typename T>
PArray<T> PArray<T>::put(const PArray& ary, size_t i, T val) {
	TRACE(std::cout << "LValue put into " << ary.data->data() << "\n");
    return put(PArray(ary), i, val);
}

// Standalone function wrappers.
template <typename T>
PArray<T> put(const PArray<T>& ary, size_t i, T val) {
    return PArray<T>::put(ary, i, val);
}
template <typename T>
PArray<T> put(PArray<T>&& ary, size_t i, T val) {
    return PArray<T>::put(std::move(ary), i, val);
}

template <typename T>
void PArray<T>::dump() const {
    std::cout << "PArray at " << data->data() << ":";
    for (const T& x : *data) {
        std::cout << " " << x;
    }
    std::cout << "\n";
}

int main() {
    PArray<int> a0 = {1, 8, 7, 2, 4};
    PArray<int> a1 = put(put(put(a0, 2, 2), 1, 1), 0, 0);
    //PArray<int> a2 = put(a1, 4, 7);
    //PArray<int> a3 = put(a2, 4, 7);
    a0.dump();
    a1.dump();
    //a2.dump();
    //a3.dump();
}
