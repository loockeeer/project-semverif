/*
 * Cours "Semantics and Application to Program Verification"
 *
 * Example inspired by A. Mine, Abstract Interpretation IV (2016)
 */

void main(){
  int i = 1;
  while (i < 15) {
    i = i + 2;
  }

  assert(i >= 15);
}
