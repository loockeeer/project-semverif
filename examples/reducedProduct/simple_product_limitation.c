/*
 * Course 13 - Reduced products
 * Simple product limitation example.
 */

void main(){
  int v = 1;

  while (v <= 10) {
    v = v + 2;
  }

  if (v >= 12) {
    v = 0;
    assert(false);
  }
}
