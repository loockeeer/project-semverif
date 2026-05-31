/*
 * Cours "Semantics and Application to Program Verification"
 *
 * Example inspired by A. Mine, Abstract Interpretation IV (2016)
 */

void main(){
  int x = rand(10, 20);
  int y = rand(0, 1);

  if (y > 0) {
    x = -x;
  }

  assert(x != 0);
}
