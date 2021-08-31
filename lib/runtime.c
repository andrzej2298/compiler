#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <limits.h>
#include <errno.h>

#define POOL_SIZE 1024

size_t string_pool_size = 0;
size_t string_pool_max_size = POOL_SIZE;
char **string_pool;

size_t object_pool_size = 0;
size_t object_pool_max_size = POOL_SIZE;
void **object_pool;

size_t flat_array_pool_size = 0;
size_t flat_array_pool_max_size = POOL_SIZE;
struct flat_array {
    int size;
    void *ptr;
};
struct flat_array *flat_array_pool;

static void __add_string_to_pool(char *str) {
    if (string_pool_size >= string_pool_max_size) {
        string_pool_max_size *= 2;
        string_pool = realloc(string_pool, string_pool_max_size * sizeof *string_pool);
    }
    string_pool[string_pool_size] = str;
    ++string_pool_size;
}

static void __add_object_to_pool(void *str) {
    if (object_pool_size >= object_pool_max_size) {
        object_pool_max_size *= 2;
        object_pool = realloc(object_pool, object_pool_max_size * sizeof *object_pool);
    }
    object_pool[object_pool_size] = str;
    ++object_pool_size;
}

static struct flat_array *__add_array_to_flat_array_pool(int size, void *ptr) {
    if (flat_array_pool_size >= flat_array_pool_max_size) {
        flat_array_pool_size *= 2;
        flat_array_pool = realloc(flat_array_pool, flat_array_pool_max_size * sizeof *flat_array_pool);
    }
    struct flat_array *struct_ptr = flat_array_pool + flat_array_pool_size;
    flat_array_pool[flat_array_pool_size].size = size;
    flat_array_pool[flat_array_pool_size].ptr = ptr;
    ++flat_array_pool_size;
    return struct_ptr;
}

void _initial_bookkeeping() {
    string_pool = calloc(string_pool_max_size, sizeof *string_pool);
    flat_array_pool = calloc(flat_array_pool_max_size, sizeof *flat_array_pool);
    object_pool = calloc(object_pool_max_size, sizeof *object_pool);
}

void _final_bookkeeping() {
    size_t i;
    for (i = 0; i < string_pool_size; ++i) {
        free(string_pool[i]);
    }
    for (i = 0; i < flat_array_pool_size; ++i) {
        free(flat_array_pool[i].ptr);
    }
    for (i = 0; i < object_pool_size; ++i) {
        free(object_pool[i]);
    }
    free(string_pool);
    free(flat_array_pool);
    free(object_pool);
}

const char *_concatenate_strings(const char *s1, const char *s2) {
    int l1 = strlen(s1), l2 = strlen(s2), concatenated_length = l1 + l2 + 1;
    char *result = calloc(concatenated_length, sizeof *result);
    strcpy(result, s1);
    strcat(result, s2);
    __add_string_to_pool(result);

    return result;
}

struct flat_array *_allocate_flat_array(int item_count, int item_size) {
    void *res = calloc(item_count, item_size);
    return __add_array_to_flat_array_pool(item_count, res);
}

struct flat_array *_allocate_object(int object_size) {
    void *res = calloc(1, object_size);
    __add_object_to_pool(res);

    return res;
}

int _array_length(struct flat_array *array) {
    return array->size;
}

void _initialize_string_array(struct flat_array *array, char *string) {
    int i;
    for (i = 0; i < array->size; ++i) {
        ((char **) array->ptr)[i] = string;
    }
}

void printInt(int i) {
    printf("%d\n", i);
}

void printString(const char *str) {
    printf("%s\n", str);
}

void error() {
    fprintf(stderr, "runtime error\n");
    _final_bookkeeping();
    exit(1);
}

int readInt() {
    int i;
    char *res = NULL;
    size_t n = 0;
    ssize_t characters_read = getline(&res, &n, stdin);
    long l = strtol(res, NULL, 10);

    if (characters_read == -1 || l > INT_MAX || l < INT_MIN) {
        i = 0;
    }
    else {
        i = l;
    }

    free(res);

    return i;
}

const char *readString() {
    char *res = NULL;
    size_t n = 0;
    ssize_t characters_read = getline(&res, &n, stdin);

    if (characters_read == -1) {
        free(res);
        error();
        return NULL;
    }
    else {
        ssize_t i;
        for (i = 0; i < characters_read; ++i) {
            if (res[i] == '\n') {
                res[i] = '\00';
            }
        }

        __add_string_to_pool(res);
        return res;
    }
}
