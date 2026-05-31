/*
 * Course 13 - Path sensitivity via manual split.
 */

void main(){
  int x = rand(-50, 50);

  if (x >= 0) {
    int y = x + 10;
    assert(y != 0); //@KO
  } else {
    int y = x - 10;
    assert(y != 0); //@KO
  }
}
