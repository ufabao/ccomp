int main(void) 
{
    int a = 2;
    while(a < 10) {
        a = a + 1;
        if (a > 6) {
          break;
        }
    }
    return a;
}