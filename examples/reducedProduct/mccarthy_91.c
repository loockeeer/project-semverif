/*
 * Course 13 - McCarthy 91 benchmark.
 */

int r;
int fuel;

void Mc(int n){
  if (n > 100) {
    r = n - 10;
  } else if (fuel > 0) {
    fuel = fuel - 1;
    Mc(n + 11);
    Mc(r);
  } else {
    r = 91;
  }
}

void main(){
  fuel = 20;
  Mc(rand(0, 120));
  assert(r >= 91);
}
