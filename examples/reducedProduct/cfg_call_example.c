/*
 * Course 13 - Inter-procedural CFG example.
 */

int R;
int X;

void f(){
  R = 2 * X;
  if (R > 100) {
    R = 0;
  }
}

void main(){
  R = -1;
  X = rand(5, 10);
  f();
  X = 80;
  f();

  assert(R == 0);
}
