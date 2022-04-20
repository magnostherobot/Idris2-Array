#include <stdlib.h>
#include <stdio.h>

size_t array_malloc_size(int n) {
    return n * sizeof(void *);
}

void **new_array() {
    size_t array_size = array_malloc_size(0);
    void **array = malloc(array_size);
    return array;
}

void **array_append_anyptr(void **array, int n, void *elem) {
    size_t new_array_size = array_malloc_size(n + 1);
    void **new_array = realloc(array, new_array_size);
    new_array[n] = elem;
    return new_array;
}

void print_array(void **array, int n) {
    printf("%i elements:\n", n);

    for (int i = 0; i < n; i++) {
        printf("  %p\n", array[i]);
    }
}

/* vim: se tw=80 ft=c sw=4 ts=4 et : */
