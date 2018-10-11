
void foo(int* p, int n) {
  __builtin_assume(n >= 8);
  p[0] = 3;
  p[n] = 4;
}

void bar(int* p, int n) {
  __builtin_assume(n % 16 == 0);
  for (int i = 0; i < n; i++) {
    p[i] = i;
  }
}

