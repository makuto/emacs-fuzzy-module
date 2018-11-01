// Created by Macoy Madson
// MIT License
// https://github.com/makuto/emacs-fuzzy-module

#include "fuzzy.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define FUZZY_MATCH_DEBUG

#ifdef FUZZY_MATCH_DEBUG
#include <stdio.h>
#endif

static bool fuzzy_match_recursive(const char* pattern, const char* str, int* outScore,
                                  const char* strBegin, unsigned char const* srcMatches,
                                  unsigned char* newMatches, int maxMatches, int nextMatch,
                                  int* recursionCount, int recursionLimit);

// Public interface
bool fuzzy_match_simple(char const* pattern, char const* str)
{
	while (*pattern != '\0' && *str != '\0')
	{
		if (tolower(*pattern) == tolower(*str))
			++pattern;
		++str;
	}

	return *pattern == '\0' ? true : false;
}

bool fuzzy_match(char const* pattern, char const* str, int* outScore)
{
	unsigned char matches[256];
	return fuzzy_match_with_matches(pattern, str, outScore, matches, sizeof(matches));
}

bool fuzzy_match_with_matches(char const* pattern, char const* str, int* outScore,
                              unsigned char* matches, int maxMatches)
{
	int recursionCount = 0;
	int recursionLimit = 10;

	return fuzzy_match_recursive(pattern, str, outScore, str, NULL, matches, maxMatches, 0,
	                             &recursionCount, recursionLimit);
}

// Private implementation
static bool fuzzy_match_recursive(const char* pattern, const char* str, int* outScore,
                                  const char* strBegin, unsigned char const* srcMatches,
                                  unsigned char* matches, int maxMatches, int nextMatch,
                                  int* recursionCount, int recursionLimit)
{
	// Count recursions
	++(*recursionCount);
	if (*recursionCount >= recursionLimit)
		return false;

	// Detect end of strings
	if (*pattern == '\0' || *str == '\0')
		return false;

	// Recursion params
	bool recursiveMatch = false;
	unsigned char bestRecursiveMatches[256];
	int bestRecursiveScore = 0;

	// Loop through pattern and str looking for a match
	bool first_match = true;
	while (*pattern != '\0' && *str != '\0')
	{
		// Found match
		if (tolower(*pattern) == tolower(*str))
		{
			// Supplied matches buffer was too short
			if (nextMatch >= maxMatches)
				return false;

			// "Copy-on-Write" srcMatches into matches
			if (first_match && srcMatches)
			{
				memcpy(matches, srcMatches, nextMatch);
				first_match = false;
			}

			// Recursive call that "skips" this match
			unsigned char recursiveMatches[256];
			int recursiveScore;
			if (fuzzy_match_recursive(pattern, str + 1, &recursiveScore, strBegin, matches,
			                          recursiveMatches, sizeof(recursiveMatches), nextMatch,
			                          recursionCount, recursionLimit))
			{
				// Pick best recursive score
				if (!recursiveMatch || recursiveScore > bestRecursiveScore)
				{
					memcpy(bestRecursiveMatches, recursiveMatches, 256);
					bestRecursiveScore = recursiveScore;
				}
				recursiveMatch = true;
			}

			// Advance
			matches[nextMatch++] = (unsigned char)(str - strBegin);
			++pattern;
		}
		++str;
	}

	// Determine if full pattern was matched
	bool matched = *pattern == '\0' ? true : false;

	// Calculate score
	if (matched)
	{
		const int sequential_bonus = 15;    // bonus for adjacent matches
		const int separator_bonus = 30;     // bonus if match occurs after a separator
		const int camel_bonus = 30;         // bonus if match is uppercase and prev is lower
		const int first_letter_bonus = 15;  // bonus if the first letter is matched

		// penalty applied for every letter in str before the first match
		const int leading_letter_penalty = -5;
		const int max_leading_letter_penalty = -15;  // maximum penalty for leading letters
		const int unmatched_letter_penalty = -1;     // penalty for every letter that doesn't matter

		// Iterate str to end
		while (*str != '\0')
			++str;

		// Initialize score
		*outScore = 100;

		// Apply leading letter penalty
		int penalty = leading_letter_penalty * matches[0];
		if (penalty < max_leading_letter_penalty)
			penalty = max_leading_letter_penalty;
		*outScore += penalty;

		// Apply unmatched penalty
		int unmatched = (int)(str - strBegin) - nextMatch;
		*outScore += unmatched_letter_penalty * unmatched;

		// Apply ordering bonuses
		for (int i = 0; i < nextMatch; ++i)
		{
			unsigned char currIdx = matches[i];

			if (i > 0)
			{
				unsigned char prevIdx = matches[i - 1];

				// Sequential
				if (currIdx == (prevIdx + 1))
					*outScore += sequential_bonus;
			}

			// Check for bonuses based on neighbor character value
			if (currIdx > 0)
			{
				// Camel case
				char neighbor = strBegin[currIdx - 1];
				char curr = strBegin[currIdx];
				if (islower(neighbor) && isupper(curr))
					*outScore += camel_bonus;

				// Separator
				bool neighborSeparator = neighbor == '_' || neighbor == ' ';
				if (neighborSeparator)
					*outScore += separator_bonus;
			}
			else
			{
				// First letter
				*outScore += first_letter_bonus;
			}
		}
	}

	// Return best result
	if (recursiveMatch && (!matched || bestRecursiveScore > *outScore))
	{
		// Recursive score is better than "this"
		memcpy(matches, bestRecursiveMatches, maxMatches);
		*outScore = bestRecursiveScore;
		return true;
	}
	else if (matched)
	{
		// "this" score is better than recursive
		return true;
	}
	else
	{
		// no match
		return false;
	}
}

#define MAX1(max, compare) max = (compare) > (max) ? (compare) : (max)

bool strRegionEquals(char const* aBegin, char const* aEnd, char const* bBegin, char const* bEnd)
{
	if (!aBegin || !aEnd || !bBegin || !bEnd)
		return false;

	if (aEnd - aBegin != bEnd - bBegin)
		return false;

	for (char const* aIter = aBegin; aIter != aEnd; ++aIter)
	{
		for (char const* bIter = bBegin; bIter != bEnd; ++bIter)
		{
			if (*aIter != *bIter)
				return false;
		}
	}

	return true;
}

bool fuzzy_match_better(char const* pattern, char const* str, int* outScore)
{
	const int initialScore = 50;
	const int firstCharacterValue = 10;
	const int acronymCharacterValue = 8;
	const int bestConsecutiveMatchMultiplier = 2;
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
					// Don't count single characters as consecutive matches
					if (consecutiveMatchStrIter - strIter > 1)
					{
						// Only count as a different consecutive match if it is equal to or larger
						// than the bestConsecutiveMatch and the string doesn't equal the previous
						// largest consecutive match. This is to prevent multiple identical matches
						// scoring highly. My test case was query "orgpro" where "org-do-promote"
						// would score lower than "org-org-export-as-org" because of all the "org"s
						if (consecutiveMatchStrIter - strIter >= bestConsecutiveMatch &&
						    !strRegionEquals(strIter, consecutiveMatchStrIter, bestMatchStrIter,
						                     bestMatchStrIterEnd))
						{
							bestMatchStrIter = strIter;
							bestMatchStrIterEnd = consecutiveMatchStrIter;
							totalConsecutiveMatches += consecutiveMatchStrIter - strIter;
						}

						// Add to the score how far we got with matches
						MAX1(bestConsecutiveMatch, consecutiveMatchStrIter - strIter);
					}

					consecutiveMatchStrIter = strIter;
				}
			}

			// Don't count single characters as consecutive matches
			if (consecutiveMatchStrIter - strIter > 1)
			{
				// Only count as a different consecutive match if it is equal to or larger than the
				// bestConsecutiveMatch and the string doesn't equal the previous largest
				// consecutive match
				if (consecutiveMatchStrIter - strIter >= bestConsecutiveMatch &&
				    !strRegionEquals(strIter, consecutiveMatchStrIter, bestMatchStrIter,
				                     bestMatchStrIterEnd))
				{
					bestMatchStrIter = strIter;
					bestMatchStrIterEnd = consecutiveMatchStrIter;
					totalConsecutiveMatches += consecutiveMatchStrIter - strIter;
				}

				// Add to the score how far we got with matches
				MAX1(bestConsecutiveMatch, consecutiveMatchStrIter - strIter);
			}
		}

		consecutiveMatchesScore = (bestConsecutiveMatch * bestConsecutiveMatchMultiplier) +
		                          (totalConsecutiveMatches * totalConsecutiveMatchesMultiplier);
	}

	// Total score. Penalize longer strings
	*outScore = (initialScore - strLength) + (acronymScore + consecutiveMatchesScore);

#ifdef FUZZY_MATCH_DEBUG
	printf("%s query %s initial %d length %d acronymScore %d consecutiveMatchesScore %d FINAL %d\n",
	       str, pattern, initialScore, strLength, acronymScore, consecutiveMatchesScore, *outScore);
#endif

	return true;
}
