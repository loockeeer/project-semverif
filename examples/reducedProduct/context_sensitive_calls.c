/*
 * Course 13 - Context sensitivity illustration.
 */

int R;

void f(int X){
  R = 2 * X;
  if (R > 100) {
    R = 0;
  }
}

void main(){
  R = -1;
  f(rand(5, 10));
  f(80);
  assert(R == 0);
}
