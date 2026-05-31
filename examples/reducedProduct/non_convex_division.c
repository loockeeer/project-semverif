/*
 * Course 13 - Disjunctive domains motivation.
 */

void main(){
  int x = rand(10, 20);
  int y = rand(0, 1);

  if (y > 0) {
    x = -x;
  }

  assert(x != 0); //@KO
  int z = 100 / x;
  assert(z <= 10); //@KO
  assert(z >= -10);
}
