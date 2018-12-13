// Created by Macoy Madson
// MIT License
// https://github.com/makuto/emacs-fuzzy-module

#include <emacs-module.h>

#include <stdlib.h>
#include <string.h>

#include "fuzzy.h"
#include "utils.h"

// Uncomment for printf debug output
/* #define MACOY_FUZZY_DEBUG */
#include <stdio.h>
#ifdef MACOY_FUZZY_DEBUG
#include <stdio.h>
#endif

int plugin_is_GPL_compatible;

// For easy switching of which algorithm to use
#define FUZZY_MATCH(query, string, outScore) fuzzy_ScoreString(query, string, outScore)

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

static emacs_value fuzzy_MakeEmptyList(emacs_env* env)
{
	emacs_value emptyList[] = {};
	return env->funcall(env, env->intern(env, "list"), 0, emptyList);
}

static emacs_value fuzzy_ProcessMatches(emacs_env* env, char* queryBuffer,
                                        MacoyFuzzyMatch matches[], int numMatches)
{
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
		free(queryBuffer);
		return fuzzy_MakeEmptyList(env);
	}
}

// Takes a query string and a vector of strings and returns a list of strings matching the query
// TODO: Make the values cached instead of having to copy the strings over every time!
static emacs_value FmacoyFuzzyFilterVector_fun(emacs_env* env, ptrdiff_t nargs, emacs_value args[],
                                               void* data)
{
	// Get the query argument
	char* queryBuffer = NULL;
	size_t queryBufferSize = 0;
	copy_string_contents(env, args[0], &queryBuffer, &queryBufferSize);

// If the third argument is set to anything, give bonus points to recently selected items
#ifdef MACOY_FUZZY_DEBUG
	printf("passed %d args", (int)nargs);
#endif
	bool isInReverseOrderOfRecentlySelected = (nargs == 3);

	// TODO: Make this resizeable or come up with a way to toss out bad scores?
	MacoyFuzzyMatch matches[MAX_MATCHES];
	int numMatches = 0;
	int numStringsToScore = env->vec_size(env, args[1]);
	// Iterate in reverse because if isInReverseOrderOfRecentlySelected is true, we want to make
	// sure we get all the best suggestions in. If that isn't true, it's still fine to go in reverse
	for (int i = numStringsToScore - 1; i >= 0; --i)
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

			// Assign history/recently selected bonus
			if (isInReverseOrderOfRecentlySelected)
			{
				// Extra bonus for very recent stuff
				if (numStringsToScore - i <= 20)
					currentMatch->score += 20 - (numStringsToScore - i);

				int inTop100Bonus = 10;
				if (numStringsToScore - i <= 100)
					currentMatch->score += inTop100Bonus;

#ifdef MACOY_FUZZY_DEBUG
				if (currentMatch->score != score)
					printf("query %s str %s score %d + history bonus %d final %d", queryBuffer,
					       stringToCheckBuffer, score, currentMatch->score - score,
					       currentMatch->score);
#endif
			}
		}
		// Reached max number of matches
		else
			break;
	}

	return fuzzy_ProcessMatches(env, queryBuffer, matches, numMatches);
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

// TODO Cache history
typedef struct FuzzyCachedList
{
	char* name;

	char* packedStrings;
	size_t* stringLengths;
	int numStrings;
} FuzzyCachedList;

#define MAX_CACHED_LISTS 10

static FuzzyCachedList g_fuzzyCachedLists[MAX_CACHED_LISTS] = {0};

static emacs_value FmacoyFuzzyCacheVector_fun(emacs_env* env, ptrdiff_t nargs, emacs_value args[],
                                              void* data)
{
	// Get the name argument
	char* nameBuffer = NULL;
	size_t nameBufferSize = 0;
	copy_string_contents(env, args[0], &nameBuffer, &nameBufferSize);

	int numStrings = env->vec_size(env, args[1]);

	char** unpackedStrings = malloc(sizeof(char*) * numStrings);
	size_t* stringLengths = malloc(sizeof(size_t) * numStrings);
	size_t packedStringsTotal = 0;

	// We're going to pack the strings contiguously. Will this help speed things up during
	// filtering? Maybe. Either way, we don't care too much if the caching code is a bit slow
	for (int i = 0; i < numStrings; ++i)
	{
		emacs_value currentString = env->vec_get(env, args[1], i);

		copy_string_contents(env, currentString, &unpackedStrings[i], &stringLengths[i]);

		// copy_string_contents() doesn't count the terminator, annoyingly
		packedStringsTotal += stringLengths[i] + 1;
	}

	// Pack strings
	char* packedStrings = malloc(packedStringsTotal);
	char* currentString = packedStrings;
	for (int i = 0; i < numStrings; i++)
	{
		strcpy(currentString, /*currentString->stringLength,*/ unpackedStrings[i]);

		// +1 for null terminator
		currentString += stringLengths[i] + 1;
	}

	free(unpackedStrings);

	// Check to see if we're updating the list or adding a new one
	FuzzyCachedList* cachedList = NULL;
	FuzzyCachedList* emptyListSlot = NULL;
	for (int i = 0; i < MAX_CACHED_LISTS; ++i)
	{
		if (!emptyListSlot && !g_fuzzyCachedLists[i].name)
			emptyListSlot = &g_fuzzyCachedLists[i];

		if (g_fuzzyCachedLists[i].name && strcmp(nameBuffer, g_fuzzyCachedLists[i].name) == 0)
		{
			cachedList = &g_fuzzyCachedLists[i];
			emptyListSlot = NULL;
			break;
		}
	}

	FuzzyCachedList* slotToUse = cachedList ? cachedList : emptyListSlot;

	if (cachedList)
	{
		free(cachedList->packedStrings);
		free(cachedList->stringLengths);
		free(cachedList->name);
	}
	// Empty list, copy its name
	if (emptyListSlot)
	{
		slotToUse->name = strdup(nameBuffer);
	}

	if (slotToUse)
	{
		slotToUse->packedStrings = packedStrings;
		slotToUse->stringLengths = stringLengths;
		slotToUse->numStrings = numStrings;
	}
	else
	{
		// Reached MAX_CACHED_LISTS; free memory and return failure
		free(packedStrings);
		free(stringLengths);
		return env->make_integer(env, 0);
	}

	return env->make_integer(env, 1);
}

static emacs_value FmacoyFuzzyFilterCachedList_fun(emacs_env* env, ptrdiff_t nargs,
                                                   emacs_value args[], void* data)
{
	// Get the cachedListName argument
	char* cachedListNameBuffer = NULL;
	size_t cachedListNameBufferSize = 0;
	copy_string_contents(env, args[0], &cachedListNameBuffer, &cachedListNameBufferSize);

	// Get the query argument
	char* queryBuffer = NULL;
	size_t queryBufferSize = 0;
	copy_string_contents(env, args[1], &queryBuffer, &queryBufferSize);

	// Get the cached list
	FuzzyCachedList* cachedList = NULL;
	for (int i = 0; i < MAX_CACHED_LISTS; ++i)
	{
		if (g_fuzzyCachedLists[i].name &&
		    strcmp(cachedListNameBuffer, g_fuzzyCachedLists[i].name) == 0)
		{
			cachedList = &g_fuzzyCachedLists[i];
			break;
		}
	}

	// Couldn't find the list by that name or empty query; early-out
	if (!cachedList || !queryBuffer || !queryBuffer[0])
		return fuzzy_MakeEmptyList(env);

	// TODO: Make this resizeable or come up with a way to toss out bad scores?
	MacoyFuzzyMatch matches[MAX_MATCHES];
	int numMatches = 0;
	int numStringsToScore = cachedList->numStrings;
	const char* currentString = cachedList->packedStrings;

	for (int i = 0; i < numStringsToScore; ++i)
	{
		const char* stringToCheck = currentString;

		int score = 0;
		bool isMatch = FUZZY_MATCH(queryBuffer, stringToCheck, &score);

		// The string didn't match at all; we won't include it in our results
		if (!isMatch)
		{
			currentString += (cachedList->stringLengths[i] + 1);
			continue;
		}

		// Add the value to our matches
		if (numMatches + 1 < MAX_MATCHES)
		{
			MacoyFuzzyMatch* currentMatch = &matches[numMatches++];

			currentMatch->string =
			    env->make_string(env, stringToCheck, cachedList->stringLengths[i]);
			currentMatch->score = score;

			currentString += (cachedList->stringLengths[i] + 1);
		}
		// Reached max number of matches
		else
			break;
	}

	return fuzzy_ProcessMatches(env, queryBuffer, matches, numMatches);
}

int emacs_module_init(struct emacs_runtime* ert)
{
	emacs_env* env = ert->get_environment(ert);

	bind_function(
	    env, "macoy-fuzzy-cache-list",
	    env->make_function(env, 2, 2, FmacoyFuzzyCacheVector_fun,
	                       "Cache the vector items in memory under the given name", NULL));

	bind_function(
	    env, "macoy-fuzzy-filter-cached-list",
	    env->make_function(env, 2, 2, FmacoyFuzzyFilterCachedList_fun,
	                       "Query the cached list which matches the given name. Works like "
	                       "macoy-filter-list-fuzzy, but doesn't support history bonus",
	                       NULL));

	bind_function(env, "macoy-filter-list-fuzzy",
	              env->make_function(
	                  env, 2, 3, FmacoyFuzzyFilterVector_fun,
	                  "Filter vector items by query and sort by score. The third argument "
	                  "specifies whether the list is ordered by recently selected (history). "
	                  "If the third argument is set to anything, macoy-filter-list-fuzzy will give "
	                  "recently selected items a bonus based on their positions in the list",
	                  NULL));

	bind_function(env, "macoy-filter-fuzzy-score",
	              env->make_function(
	                  env, 2, 2, FmacoyFuzzyScore_fun,
	                  "(query, string). Returns the score of the string based on query", NULL));

	provide(env, "macoy-fuzzy");

	return 0;
}
