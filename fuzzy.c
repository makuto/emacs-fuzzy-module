// Created by Macoy Madson
// MIT License
// https://github.com/makuto/emacs-fuzzy-module

#include "fuzzy.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

/* #define FUZZY_MATCH_DEBUG */

#ifdef FUZZY_MATCH_DEBUG
#include <stdio.h>
#endif

static bool fuzzy_StringRegionEquals(char const* aBegin, char const* aEnd, char const* bBegin,
                                     char const* bEnd)
{
	if (!aBegin || !aEnd || !bBegin || !bEnd)
		return false;

	if (aEnd - aBegin != bEnd - bBegin)
		return false;

	char const* bIter = bBegin;
	for (char const* aIter = aBegin; aIter != aEnd && bIter != bEnd; ++aIter, ++bIter)
	{
		if (*aIter != *bIter)
			return false;
	}

	return true;
}

static int fuzzy_Max(int a, int b)
{
	return a > b ? a : b;
}

static void fuzzy_ScoreString_ConsecutiveMatch(
    char const* strIter, char const* consecutiveMatchStrIter, int* bestConsecutiveMatch,
    char const** bestMatchStrIter, char const** bestMatchStrIterEnd, int* totalConsecutiveMatches)
{
	// Don't count single characters as consecutive matches
	if (consecutiveMatchStrIter - strIter > 1)
	{
		// Only count as a different consecutive match if it is equal to or larger than the
		// bestConsecutiveMatch and the string doesn't equal the previous largest consecutive match.
		// This is to prevent multiple identical matches scoring highly
		if (consecutiveMatchStrIter - strIter >= *bestConsecutiveMatch &&
		    !fuzzy_StringRegionEquals(strIter, consecutiveMatchStrIter, *bestMatchStrIter,
		                              *bestMatchStrIterEnd))
		{
			*bestMatchStrIter = strIter;
			*bestMatchStrIterEnd = consecutiveMatchStrIter;
			*totalConsecutiveMatches += consecutiveMatchStrIter - strIter;
		}

		// Add to the score how far we got with matches
		*bestConsecutiveMatch = fuzzy_Max(*bestConsecutiveMatch, consecutiveMatchStrIter - strIter);
	}
}

bool fuzzy_ScoreString(char const* pattern, char const* str, int* outScore)
{
	const int initialScore = 50;
	const int firstCharacterValue = 10;
	const int acronymCharacterValue = 10;
	const int bestConsecutiveMatchMultiplier = 2;
	const float perfectPrefixMatchMultiplier = 1.2f;
	// Note that totalConsecutiveMatches will not be a a linear value, i.e. if you have query 'tes'
	// and you're scoring 'tester' totalConsecutiveMatches will be 7 ('tes', 'es', 'te')
	const int totalConsecutiveMatchesMultiplier = 1;

	// First, make sure we have all characters in pattern
	{
		char const* patternIter = pattern;
		char const* strIter = str;

		while (*patternIter != '\0' && *strIter != '\0')
		{
			if (tolower(*patternIter) == tolower(*strIter))
				++patternIter;
			++strIter;
		}

		if (*patternIter != '\0')
			return false;
	}

	// Score acronyms
	int acronymScore = 0;
	{
		char const* patternIter = pattern;
		for (char const* strIter = str; *strIter != '\0' && *patternIter != '\0'; ++strIter)
		{
			if (tolower(*patternIter) == tolower(*strIter))
			{
				// First character matches get bonus
				if (strIter == str)
					acronymScore += firstCharacterValue;
				// Handle underscore_delimited, camelCase, lisp-style, and file/dir/paths
				else if (*(strIter - 1) == '_' || *(strIter - 1) == '/' || *(strIter - 1) == '-' ||
				         (isupper(*strIter) && islower(*(strIter - 1))))
					acronymScore += acronymCharacterValue;

				patternIter++;
			}
		}
	}

	// Score consecutive matches. We'll record the string length here too to save on a strlen() call
	int consecutiveMatchesScore = 0;
	int strLength = 0;
	{
		// This is going to be expensive; for each character in string, restart the pattern and see
		// the best consecutive match we can get
		int bestConsecutiveMatch = 0;
		char const* bestMatchStrIter = NULL;
		char const* bestMatchStrIterEnd = NULL;
		// We'll use the total to approximate a smarter algorithm which would understand two
		// disjoint consecutive matches. We don't want to figure out how to do that so we'll just
		// make sure those will outscore less match-y things. Best match still has priority though
		int totalConsecutiveMatches = 0;
		for (char const* strIter = str; *strIter != '\0'; ++strIter)
		{
			++strLength;

			char const* consecutiveMatchStrIter = strIter;
			for (char const* patternIter = pattern;
			     *consecutiveMatchStrIter != '\0' && *patternIter != '\0'; ++patternIter)
			{
				if (tolower(*patternIter) == tolower(*consecutiveMatchStrIter))
					++consecutiveMatchStrIter;
				else
				{
					fuzzy_ScoreString_ConsecutiveMatch(
					    strIter, consecutiveMatchStrIter, &bestConsecutiveMatch, &bestMatchStrIter,
					    &bestMatchStrIterEnd, &totalConsecutiveMatches);

					consecutiveMatchStrIter = strIter;
				}
			}

			fuzzy_ScoreString_ConsecutiveMatch(strIter, consecutiveMatchStrIter,
			                                   &bestConsecutiveMatch, &bestMatchStrIter,
			                                   &bestMatchStrIterEnd, &totalConsecutiveMatches);
		}

		// If the best match is the first characters, give an extra bonus
		int perfectPrefixMatch = 0;
		int bestMatchLength = bestMatchStrIterEnd - bestMatchStrIter;
		if (fuzzy_StringRegionEquals(str, str + bestMatchLength, bestMatchStrIter,
		                             bestMatchStrIterEnd))
			perfectPrefixMatch = bestMatchLength * perfectPrefixMatchMultiplier;

		consecutiveMatchesScore = (bestConsecutiveMatch * bestConsecutiveMatchMultiplier) +
		                          (totalConsecutiveMatches * totalConsecutiveMatchesMultiplier) +
		                          perfectPrefixMatch;
	}

	// Total score. Penalize longer strings
	*outScore = (initialScore - strLength) + (acronymScore + consecutiveMatchesScore);

#if FUZZY_MATCH_DEBUG
	printf("%s query %s initial %d length %d acronymScore %d consecutiveMatchesScore %d FINAL %d\n",
	       str, pattern, initialScore, strLength, acronymScore, consecutiveMatchesScore, *outScore);
#endif

	return true;
}
