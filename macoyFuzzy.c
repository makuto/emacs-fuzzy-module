#include <emacs-module.h>

#include <stdlib.h>

#include "fuzzy.h"
#include "utils.h"

int plugin_is_GPL_compatible;

// Note that for now this will affect the quality of the results if e.g. the best result is actually
// match #2049. I'll have to make this good eventually
#define MAX_MATCHES 2048

typedef struct MacoyFuzzyMatch
{
	emacs_value string;
	int score;
} MacoyFuzzyMatch;

// Return the value of the given element
typedef int (*quicksort_GetValueFunc)(void* value);
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

	quicksort(array, i, getValue);
	quicksort(array + i, length - i, getValue);
}

int getFuzzyMatchValue(void* match)
{
	return ((MacoyFuzzyMatch*)match)->score;
}

// Make sure to free the returned array
MacoyFuzzyMatch** sortFuzzyMatches(MacoyFuzzyMatch matches[], ptrdiff_t numMatches)
{
	MacoyFuzzyMatch** sortedMatches = malloc(sizeof(MacoyFuzzyMatch*) * numMatches);
	for (int i = 0; i < numMatches; ++i)
		sortedMatches[i] = &matches[i];

	quicksortReverse((void**)sortedMatches, numMatches, getFuzzyMatchValue);
	return sortedMatches;
}

emacs_value makeListFromFuzzyMatches(emacs_env* env, MacoyFuzzyMatch** matches, ptrdiff_t numElems)
{
	emacs_value* values = malloc(sizeof(emacs_value) * numElems);
	for (int i = 0; i < numElems; ++i)
		values[i] = matches[i]->string;

	emacs_value listObject = env->funcall(env, env->intern(env, "list"), numElems, values);
	free(values);
	return listObject;
}

// Takes a query string and a vector of strings and returns a list of strings matching the query
// TODO: Make the values cached instead of having to copy the strings over every time!
static emacs_value FmacoyFuzzyFilterVector_fun(emacs_env* env, ptrdiff_t nargs, emacs_value args[],
                                               void* data)
{
	// Get the string arguments
	char* queryBuffer = NULL;
	size_t queryBufferSize = 0;
	copy_string_contents(env, args[0], &queryBuffer, &queryBufferSize);

	// TODO: Make this resizeable or come up with a way to toss out bad scores?
	MacoyFuzzyMatch matches[MAX_MATCHES];
	int numMatches = 0;

	for (int i = 0; i < env->vec_size(env, args[1]); ++i)
	{
		emacs_value currentString = env->vec_get(env, args[1], i);

		char* stringToCheckBuffer = NULL;
		size_t stringToCheckBufferSize = 0;
		copy_string_contents(env, currentString, &stringToCheckBuffer, &stringToCheckBufferSize);

		int score = 0;
		bool isMatch = fuzzy_match(queryBuffer, stringToCheckBuffer, &score);

		free(stringToCheckBuffer);

		// The string didn't match at all; we won't include it in our results
		if (!isMatch)
			continue;

		// Add the value to our matches
		if (numMatches + 1 < MAX_MATCHES)
		{
			MacoyFuzzyMatch* currentMatch = &matches[numMatches++];
			currentMatch->string = currentString;
			currentMatch->score = score;
		}
		// Reached max number of matches
		else
		    break;
	}

	free(queryBuffer);

	if (numMatches)
	{
		MacoyFuzzyMatch** sortedMatches = sortFuzzyMatches(matches, numMatches);

		emacs_value matchesList = makeListFromFuzzyMatches(env, sortedMatches, numMatches);
		free(sortedMatches);
		return matchesList;
	}
	else
	{
		emacs_value emptyList[] = {};
		return env->funcall(env, env->intern(env, "list"), 0, emptyList);
	}
}

static emacs_value FmacoyFuzzyScore_fun(emacs_env* env, ptrdiff_t nargs, emacs_value args[],
                                        void* data)
{
	// Get the string arguments
	char* queryBuffer = NULL;
	size_t queryBufferSize = 0;
	char* stringToCheckBuffer = NULL;
	size_t stringToCheckBufferSize = 0;
	copy_string_contents(env, args[0], &queryBuffer, &queryBufferSize);
	copy_string_contents(env, args[1], &stringToCheckBuffer, &stringToCheckBufferSize);

	int outScore = 0;
	fuzzy_match(queryBuffer, stringToCheckBuffer, &outScore);

	free(queryBuffer);
	free(stringToCheckBuffer);

	return env->make_integer(env, outScore);
}

int emacs_module_init(struct emacs_runtime* ert)
{
	emacs_env* env = ert->get_environment(ert);
	bind_function(env, "macoy-filter-list-fuzzy",
	              env->make_function(env, 2, 2, FmacoyFuzzyFilterVector_fun,
	                                 "Filter vector items by query and sort by score.", NULL));
	bind_function(env, "macoy-filter-fuzzy-score",
	              env->make_function(
	                  env, 2, 2, FmacoyFuzzyScore_fun,
	                  "(query, string). Returns the score of the string based on query", NULL));
	provide(env, "macoy-fuzzy");
	return 0;
}

/* Integration
Follow flx-ido defadvice for ido to replace (esp ido-set-matches-1)
*/
