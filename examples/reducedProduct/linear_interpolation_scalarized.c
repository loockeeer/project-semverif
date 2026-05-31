/*
 * Course 13 - Linear interpolation inspired benchmark.
 * Scalarized form (no arrays): 3 table segments.
 */

void main(){
  int x = rand(0, 30);
  int i = 0;
  int y = 0;

  while (i < 3 && x > (i + 1) * 10) {
    i = i + 1;
  }

  if (i == 0) {
    y = 100 + x * 2;
  } else if (i == 1) {
    y = 120 + (x - 10) * 3;
  } else {
    y = 150 + (x - 20) * 4;
  }

  assert(y >= 100);
}
