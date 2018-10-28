#pragma once

#include <stdbool.h>

#include <emacs-module.h>

// Free() the buffer once you're done with it
bool copy_string_contents(emacs_env* env, emacs_value value, char** buffer, size_t* size);

/* Bind NAME to FUN.  */
void bind_function(emacs_env* env, const char* name, emacs_value Sfun);

/* Provide FEATURE to Emacs.  */
void provide(emacs_env* env, const char* feature);
