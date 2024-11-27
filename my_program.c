// my_program.c
#include <stdio.h>

int myFunction(int arg) {
    printf("Called from OCaml with argument: %d\n", arg);
    return arg * 2; // Example functionality
}
