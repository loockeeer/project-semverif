/*
 * Course 13 - Path sensitivity intuition.
 */

void main(){
  int x = rand(-50, 50);
  int y = 0;

  if (x >= 0) {
    y = x + 10;
  } else {
    y = x - 10;
  }

  assert(y != 0); //@KO
}
