int foo(int a, int b){
  for(int i = 0; i < 10; i = i + 1){
    a = a + b;
  }
  return a;
}

int main(void){
    return foo(1, 1);
}