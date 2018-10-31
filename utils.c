#include "utils.h"

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <emacs-module.h>

// From https://phst.github.io/emacs-modules#introduction
bool copy_string_contents(emacs_env* env, emacs_value value, char** buffer, size_t* size)
{
	ptrdiff_t buffer_size;
	if (!env->copy_string_contents(env, value, NULL, &buffer_size))
		return false;
	assert(env->non_local_exit_check(env) == emacs_funcall_exit_return);
	assert(buffer_size > 0);
	*buffer = malloc((size_t)buffer_size);
	if (*buffer == NULL)
	{
		env->non_local_exit_signal(env, env->intern(env, "memory-full"), env->intern(env, "nil"));
		return false;
	}
	ptrdiff_t old_buffer_size = buffer_size;
	if (!env->copy_string_contents(env, value, *buffer, &buffer_size))
	{
		free(*buffer);
		*buffer = NULL;
		return false;
	}
	assert(env->non_local_exit_check(env) == emacs_funcall_exit_return);
	assert(buffer_size == old_buffer_size);
	*size = (size_t)(buffer_size - 1);
	return true;
}

/* Bind NAME to FUN.  */
void bind_function(emacs_env* env, const char* name, emacs_value Sfun)
{
	emacs_value Qfset = env->intern(env, "fset");
	emacs_value Qsym = env->intern(env, name);
	emacs_value args[] = {Qsym, Sfun};

	env->funcall(env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
void provide(emacs_env* env, const char* feature)
{
	emacs_value Qfeat = env->intern(env, feature);
	emacs_value Qprovide = env->intern(env, "provide");
	emacs_value args[] = {Qfeat};

	env->funcall(env, Qprovide, 1, args);
}

// Sorts in place (modifies array)
// Modified from https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C (GNU FDL license)
void quicksort(void** array, int length, quicksort_GetValueFunc getValue)
{
	if (length < 2)
		return;

	int pivot = getValue(array[length / 2]);

	int i, j;
	for (i = 0, j = length - 1;; i++, j--)
	{
		while (getValue(array[i]) < pivot)
			i++;
		while (getValue(array[j]) > pivot)
			j--;

		if (i >= j)
			break;

		void* temp = array[i];
		array[i] = array[j];
		array[j] = temp;
	}

	quicksort(array, i, getValue);
	quicksort(array + i, length - i, getValue);
}

// Sorts in place (modifies array)
// Modified from https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#C (GNU FDL license)
void quicksortReverse(void** array, int length, quicksort_GetValueFunc getValue)
{
	if (length < 2)
		return;

	int pivot = getValue(array[length / 2]);

	int i, j;
	for (i = 0, j = length - 1;; i++, j--)
	{
		while (getValue(array[i]) > pivot)
			i++;
		while (getValue(array[j]) < pivot)
			j--;

		if (i >= j)
			break;

		void* temp = array[i];
		array[i] = array[j];
		array[j] = temp;
	}

	quicksortReverse(array, i, getValue);
	quicksortReverse(array + i, length - i, getValue);
}
