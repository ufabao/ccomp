int main(void) 
{
    int a = 2;
    int b = 3;
    while(a < 10) {
        while (b < 6){
          b = b + 1;
          continue;
          a = -1;
        }
        a = a + 1;
    }
    return a;
}