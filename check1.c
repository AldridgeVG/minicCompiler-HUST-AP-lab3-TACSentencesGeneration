int main() {
  int a, b, c;
  a = 10;
  b = 20;
  c = 30;
  c = ++a + ++a + b++ + b++;
  c = a == b || a > 10 && b < 20;
  return 1;
}