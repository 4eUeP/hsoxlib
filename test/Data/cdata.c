#include <stdio.h>

char *hello_cstring(void) {
  static char *hello = "hello";
  return hello;
}

char **hello_cstrings(void) {
  static char *hello[] = {"hello", "world", NULL};
  return hello;
}

char **hello_cstrings_0(void) {
  static char *hello[] = {"hello", "world", "\0", NULL};
  return hello;
}

char **empty_cstrings(void) {
  static char *xs[] = {NULL};
  return xs;
}

double *cdouble_3_14(void) {
  static double hello = 3.14;
  return &hello;
}
