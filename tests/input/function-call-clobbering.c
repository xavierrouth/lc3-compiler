int one() {
  return 1;
}

int main() {
  int a = one() + one() + one() + one() + one();
  return a;
}