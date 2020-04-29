#include <stdio.h>
#include <inttypes.h>
#include <stdlib.h>

#define typeof_mask  0x3
#define val_shift    0x2
#define type_fixnum  0x0
#define type_bool    0x1
#define type_char    0x2

int64_t entry();
void print_char(int64_t);

int main(int argc, char** argv) {
  int64_t result = entry();
  switch (typeof_mask & result) {
  case type_fixnum:    
    printf("%" PRId64 "\n", result >> val_shift);
    break;
  case type_bool:
    printf("#%c\n", result >> val_shift ? 't' : 'f');
    break;
  case type_char:
    print_char(result);
  }  
  return 0;
}

void error() {
  printf("err");
  exit(1);
}
 
void print_char (int64_t v) {
  int64_t codepoint = v >> val_shift;
  printf("#\\");        
  switch (codepoint) {
  case 0:
    printf("nul"); break;
  case 8:
    printf("backspace"); break;
  case 9:
    printf("tab"); break;
  case 10:
    printf("newline"); break;
  case 11:
    printf("vtab"); break;
  case 12:
    printf("page"); break;
  case 13:
    printf("return"); break;
  case 32:
    printf("space"); break;
  case 127:
    printf("rubout"); break;    
  default:
    // Print using UTF-8 encoding of codepoint
    // https://en.wikipedia.org/wiki/UTF-8
    if (codepoint < 128) {
      printf("%c", (char) codepoint);	
    } else if (codepoint < 2048) {
      printf("%c%c",
	     (char)(codepoint >> 6) | 192,
	     ((char)codepoint & 63) | 128);
    } else if (codepoint < 65536) {
      printf("%c%c%c",
	     (char)(codepoint >> 12) | 224,
	     ((char)(codepoint >> 6) & 63) | 128,
	     ((char)codepoint & 63) | 128);
    } else {
      printf("%c%c%c%c",	
	     (char)(codepoint >> 18) | 240,
	     ((char)(codepoint >> 12) & 63) | 128,
	     ((char)(codepoint >> 6) & 63) | 128,
	     ((char)codepoint & 63) | 128);
    }
  }
  printf("\n");
}

