// Created by Macoy Madson
// GPL V3
// https://github.com/makuto/emacs-fuzzy-module

#include <emacs-module.h>

// remove (for printf)
#include <stdio.h>

#include <stdlib.h>

#include "fuzzy.h"
#include "utils.h"

int plugin_is_GPL_compatible;

// Uncomment for printf debug output
//#define MACOY_FUZZY_DEBUG

#define FUZZY_MATCH(query, string, outScore) fuzzy_match_better(query, string, outScore)

// Note that for now this will affect the quality of the results if e.g. the best result is actually
// match #2049. I'll have to make this good eventually
#define MAX_MATCHES 2048

typedef struct MacoyFuzzyMatch
{
	emacs_value string;
	int score;
} MacoyFuzzyMatch;

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
		bool isMatch = FUZZY_MATCH(queryBuffer, stringToCheckBuffer, &score);

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

	if (numMatches)
	{
		MacoyFuzzyMatch** sortedMatches = sortFuzzyMatches(matches, numMatches);

#ifdef MACOY_FUZZY_DEBUG
		printf("\nQuery: %s\n", queryBuffer);
		for (int i = 0; i < numMatches; ++i)
		{
			char* stringBuffer = NULL;
			size_t stringBufferSize = 0;
			copy_string_contents(env, sortedMatches[i]->string, &stringBuffer, &stringBufferSize);
			printf("%s score: %d\n", stringBuffer, sortedMatches[i]->score);
			free(stringBuffer);
		}
#endif

		emacs_value matchesList = makeListFromFuzzyMatches(env, sortedMatches, numMatches);
		free(sortedMatches);

		free(queryBuffer);
		return matchesList;
	}
	else
	{
		emacs_value emptyList[] = {};
		free(queryBuffer);
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
	FUZZY_MATCH(queryBuffer, stringToCheckBuffer, &outScore);

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
