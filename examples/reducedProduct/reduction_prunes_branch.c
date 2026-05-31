/*
 * Course 13 - Reduced products
 * Reduction should prune the then branch.
 */

void main(){
  int v = 1;

  while (v <= 10) {
    v = v + 2;
  }

  if (v >= 12) {
    assert(false);
  }

  assert(v == 11);
}
