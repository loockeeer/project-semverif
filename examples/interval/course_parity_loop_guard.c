/*
 * Cours "Semantics and Application to Program Verification"
 *
 * Example inspired by A. Mine, Abstract Interpretation IV (2016)
 */

void main(){
  int v = 1;
  while (v <= 10) {
    v = v + 2;
  }

  if (v >= 12) {
    assert(false);
  }
}
