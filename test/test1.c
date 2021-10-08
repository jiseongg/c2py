void print_1() {
  int a;
  a = 1;
  print (a);
}

int main() {
  int i;
  int[1000] a;

  while (i < 1000) {
    i++;
  }
  a[i] = 1; /* bug: 'i' should be 'i-1' */
}
