// my_program.c

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <math.h>
#include <stdint.h>

double pack_chars_to_float(const char *chars) {
    double packed = 0.0;
    for (int i = 0; i < 7; i++) {
        packed += ldexp((double)(chars[i] & 0x7F), (7 - i) * 7);
    }
    return packed;
}

void unpack_float_to_chars(double packed, char *chars) {
    for (int i = 0; i < 7; i++) {
        int char_val = (int)(packed / ldexp(1.0, (7 - i) * 7)) & 0x7F;
        chars[i] = (char)char_val;
        packed -= char_val * ldexp(1.0, (7 - i) * 7);
    }
    chars[7] = '\0'; // Null-terminate the string
}

int myFunction(int arg) {
    printf("Called from OCaml with argument: %d\n", arg);
    return arg * 2; // Example functionality
}

double myFloat(double arg1, double arg2) {
  double quotient = arg1/arg2;
  printf("Called from OCaml with argument: %f, %f, %f\n", arg1, arg2, quotient);
    return quotient; // Example functionality
}

void float_to_string_sub(char *first, double f) {
  double flr = floor (f / 128.0);
  double fprime = f - flr * 128.0;
  int len;
  if (flr > 0.0) float_to_string_sub(first, flr);
  len = strlen(first);
  first[len] = (int) fprime;
  first[len+1] = 0;
}

char *float_to_string(double f) {
  char buf[20];
  *buf = 0;
  float_to_string_sub(buf, f);
  return strdup(buf);
}

void myAscii(double asciif) {
  if (0) printf("Received double was %f\n", asciif);
  char *str = float_to_string(asciif);
  printf("Received string: %s\n", str);
}
