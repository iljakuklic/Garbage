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

// Typed (sort of) version of parray_alloc.
#define parray_new(type, nelems) (parray_alloc(nelems * sizeof(type)))
// Get an element of particular type.
// The access must be in bounds. Setting an element through this might violate
// parray invariants, use parray_put instead (except for first initialization).
#define parray_data(type, ary) ((type*)(ary)->data)
#define parray_element(type, ary, i) (parray_data(type, ary)[i])
// Get number of elements of given type.
#define parray_size(type, ary) ((ary)->size / sizeof(type))
// Put an element into the array.
#define parray_put(type, ary, i, val) ({ type elem = (val); \
    parray_putraw(ary, (char*)&elem, sizeof(type) * i, sizeof(type)); })

// Persisten array internals.
struct parray {
    unsigned refcount;
    size_t size;
    char data[];
};

// Create an uninitialized persistent array.
struct parray* parray_alloc(size_t size) {
    struct parray* ary = malloc(sizeof(struct parray) + size);
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
struct parray* parray_putraw(struct parray* ary, const char* new_value,
                             size_t offset, size_t elem_size) {
    assert(ary->refcount > 0 && "Accessing a freed array");
    assert(offset + elem_size <= ary->size && "Out of bounds access");

    if (ary->refcount == 1) {
        // We have the only reference to the array, can update in place.
        TRACE("Array %p: unique reference\n", ary);
        memcpy(ary->data + offset, new_value, elem_size);
        return ary;
    }

    if (memcmp(ary->data + offset, new_value, elem_size) == 0) {
        // The already present value is the same as the one about to be written,
        // no need to copy the array.
        TRACE("Array %p: value already present\n", ary);
        return ary;
    }

    // Otherwise, allocate a new array, copy elements over and update
    // the element to be written.
    struct parray* new_ary = parray_alloc(ary->size);
    memcpy(new_ary->data, ary->data, ary->size);
    memcpy(new_ary->data + offset, new_value, elem_size);
    TRACE("Copy %p to %p\n", ary, new_ary);

    // Release one reference of the original array.
    ary->refcount--;

    return new_ary;
}

// Dump a parray of integers.
void parray_dump_int(struct parray* ary) {
    int nelem = parray_size(int, ary);
    printf("Array %p: [%d]", ary, nelem);
    for (int i = 0; i < nelem; i++) {
        printf(" %d", parray_element(int, ary, i));
    }
    printf("\n");
}

int main(void) {
    struct parray* ary1 = parray_new(int, 5);
    parray_element(int, ary1, 0) = 7;
    parray_element(int, ary1, 1) = 2;
    parray_element(int, ary1, 2) = 9;
    parray_element(int, ary1, 3) = 4;
    parray_element(int, ary1, 4) = 3;
    struct parray* ary2 = parray_keep(ary1);
    ary2 = parray_put(int, ary2, 2, 7);
    ary2 = parray_put(int, ary2, 3, 6);
    ary2 = parray_put(int, ary2, 3, 7);
    ary2 = parray_put(int, ary2, 3, 8);
    parray_dump_int(ary1);
    parray_dump_int(ary2);
    parray_unref(ary1);
    parray_unref(ary2);
}
