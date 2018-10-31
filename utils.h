#pragma once

#include <stdbool.h>

#include <emacs-module.h>

//
// Emacs helpers
//

// Free() the buffer once you're done with it
bool copy_string_contents(emacs_env* env, emacs_value value, char** buffer, size_t* size);

/* Bind NAME to FUN.  */
void bind_function(emacs_env* env, const char* name, emacs_value Sfun);

/* Provide FEATURE to Emacs.  */
void provide(emacs_env* env, const char* feature);

//
// Quicksort
//

// Return the value of the given element
typedef int (*quicksort_GetValueFunc)(void* value);

// Sorts in place (modifies array)
// Modified from https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C (GNU FDL license)
void quicksort(void** array, int length, quicksort_GetValueFunc getValue);

// Sorts in place (modifies array)
// Modified from https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C (GNU FDL license)
void quicksortReverse(void** array, int length, quicksort_GetValueFunc getValue);
