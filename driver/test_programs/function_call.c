int foo(int a, int b){
  return a + b;
}

int main(void) {
  return foo(foo(1, 2), 3);
}