int main(void) 
{
    int a = 1;
    {
      int b = 2;
      a = b;
    }
  return a;
}