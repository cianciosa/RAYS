//******************************************************************************
///  @file backtrace.c
///  @brief Functions to fast copy files.
///
///  Routines to print out stack traces.
//******************************************************************************

#include <execinfo.h>
#include <stdio.h>
#include <stdlib.h>

//------------------------------------------------------------------------------
///  @brief Print out a stacktrace then abort the program.
//------------------------------------------------------------------------------
void abort() {
    void *buffer = malloc(BACKTRACE_SIZE*sizeof(void *));
    int num_pointers = backtrace(buffer, BACKTRACE_SIZE);

    char **strings = backtrace_symbols(buffer, num_pointers);
    if (strings) {
        for (int i = 0; i < num_pointers; i++) {
            printf("%s\n", strings[i]);
        }
    }

    free(strings);
    exit(1);
}
