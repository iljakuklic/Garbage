#include <assert.h>
#include <memory.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

// Helper for debugging.
#ifdef NOTRACE
#define TRACE(...) do { } while (false)
#else
#define TRACE(...) do { printf(__VA_ARGS__); } while (false)
#endif

#define ELEM int

// Persisten array internals.
struct parray {
    size_t refcount;
    size_t size;
    ELEM data[];
};

// Create an uninitialized persistent array.
struct parray* parray_new(size_t size) {
    size_t data_size = sizeof(ELEM) * size;
    struct parray* ary = malloc(sizeof(struct parray) + data_size);
    ary->refcount = 1;
    ary->size = size;
    TRACE("Array %p: alloc\n", ary);
    return ary;
}

// Mark a reference as dead. If no other references remain, delete the array.
void parray_unref(struct parray* ary) {
    assert(ary->refcount > 0 && "Double free");
    if (--ary->refcount == 0) {
        TRACE("Array %p: free\n", ary);
        free(ary);
    }
}

// Keep an existing version of persistent array.
// If this is not being called before update, the pointer to the array will
// be invalidated by passing it to the function.
struct parray* parray_keep(struct parray* ary) {
    assert(ary->refcount > 0 && "Accessing a freed array");
    ary->refcount++;
    return ary;
}

// Update an element in the array without modifying the original array.
// (Unless we have the only reference to it or the operation does not modify
// the array contents.)
struct parray* parray_put(struct parray* ary, size_t i, ELEM value) {
    assert(ary->refcount > 0 && "Accessing a freed array");
    assert(i < ary->size && "Out of bounds access");

    if (ary->refcount == 1) {
        // We have the only reference to the array, can update in place.
        TRACE("Array %p: unique reference\n", ary);
        ary->data[i] = value;
        return ary;
    }

    if (ary->data[i] == value) {
        // The already present value is the same as the one about to be written,
        // no need to copy the array.
        TRACE("Array %p: value already present\n", ary);
        return ary;
    }

    // Otherwise, allocate a new array, copy elements over and update
    // the element to be written.
    struct parray* new_ary = parray_new(ary->size);
    memcpy(new_ary->data, ary->data, ary->size * sizeof(ELEM));
    new_ary->data[i] = value;
    TRACE("Copy %p to %p\n", ary, new_ary);

    // Release one reference of the original array.
    ary->refcount--;

    return new_ary;
}

// Dump a parray of integers.
__attribute__((noinline))
void parray_dump(struct parray* ary) {
    printf("Array: [%d]", ary->size);
    for (int i = 0; i < ary->size; i++) {
        printf(" %d", ary->data[i]);
    }
    printf("\n");
}

struct sortstate { struct parray* ary; size_t begin; size_t end; };

struct sortstate partition_s(struct sortstate s, int pivot) {
    if (s.end <= s.begin) {
        return s;
    }
    if (s.ary->data[s.begin] < pivot) {
        struct sortstate s1 = { s.ary, s.begin + 1, s.end };
        return partition_s(s1, pivot);
    }
    if (s.ary->data[s.end - 1] > pivot) {
        struct sortstate s1 = { s.ary, s.begin, s.end - 1 };
        return partition_s(s1, pivot);
    }
    int beginval = s.ary->data[s.begin];
    int endval = s.ary->data[s.end - 1];
    TRACE("Swapping %p %d (%d) %d\n", s.ary, beginval, pivot, endval);
    struct parray* ary1 = parray_put(s.ary, s.begin, endval);
    struct parray* ary2 = parray_put(ary1, s.end - 1, beginval);
    struct sortstate s1 = { ary2, s.begin + 1, s.end - 1 };
    return partition_s(s1, pivot);
}

struct sortstate quicksort_s(struct sortstate s0) {
    if (s0.end <= s0.begin + 1) {
        return s0;
    }
    
    TRACE("Sorting %p [%lu, %lu)\n", s0.ary, s0.begin, s0.end);
    
    struct sortstate s1 = partition_s(s0, s0.ary->data[s0.begin]);
    parray_dump(s1.ary);
    struct sortstate s2 = { s1.ary, s0.begin, s1.end };
    struct sortstate s3 = quicksort_s(s2);
    struct sortstate s4 = { s3.ary, s1.begin, s0.end };
    struct sortstate s5 = quicksort_s(s4);
    return s5;
}

struct parray* quicksort(struct parray* ary) {
    struct sortstate s0 = { ary, 0, ary->size };
    struct sortstate s1 = quicksort_s(s0);
    return s1.ary;
}

int main(void) {
    struct parray* ary1 = parray_new(8);
    ary1->data[0] = 7;
    ary1->data[1] = 2;
    ary1->data[2] = 9;
    ary1->data[3] = 4;
    ary1->data[4] = 11;
    ary1->data[5] = 2;
    ary1->data[6] = 71;
    ary1->data[7] = 0;
    parray_dump(ary1);
    ary1 = quicksort(ary1);
    parray_dump(ary1);
    
    parray_unref(ary1);
}
