  $ echo "{define x of int8 = 1;}" > program.mylang
  $ main program.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    static int8_t x = 1;
    return 0;
  }


  $ main valid/match.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    static int8_t x = 5;
    switch (x) {
    case 2:
      printf("%d\n", 2);
      break;
    case 5:
      printf("%d\n", 5);
      break;
    case 6:
      printf("%d\n", 6);
      break;
    default:
      printf("%d\n", 0);
      break;
    }
    return 0;
  }

  $ main valid/function.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    int32_t function_plus(int32_t a, int32_t b)  {
      static int32_t x = 0;
      return a + b + x;
    }
    static int32_t x = 10;
    static int32_t y = 20;
    static int32_t z = function_plus(x, y);
    return 0;
  }

  $ main valid/mutimut.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    static int32_t x = 10;
    int32_t y = 20;
    y = 30;
    printf("%d\n", x);
    printf("%d\n", y);
    return 0;
  }

  $ main valid/ifelse.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    if (5 > 0)
    {
      printf("%d\n", 1);
  
    }
    else
    {
      printf("%d\n", 0);
  
    }
    return 0;
  }

  $ main valid/bufinit.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    int32_t my_buffer_data[128];
    int my_buffer_len = 0;
    return 0;
  }

  $ main valid/bufwrite.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    int32_t my_buffer_data[128];
    int my_buffer_len = 0;
    static int32_t w1 = my_buffer_data[my_buffer_len++] = 42;
    static int32_t w2 = my_buffer_data[my_buffer_len++] = 99;
    return 0;
  }

  $ main valid/bufread.mylang
  #include <stdio.h>
  #include <stdint.h>
  #include <math.h>
  
  int main(void)
  {
    int32_t my_buffer_data[128];
    int my_buffer_len = 0;
    static int32_t w1 = my_buffer_data[my_buffer_len++] = 42;
    static int32_t x = my_buffer_data[--my_buffer_len];
    printf("%d\n", x);
    return 0;
  }




