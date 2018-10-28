#include "fuzzy.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

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
