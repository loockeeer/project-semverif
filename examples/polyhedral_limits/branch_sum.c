/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int b = rand(0, 1);
  int x;
  int y;

  if(b == 0) {
    x = 0;
    y = 2;
  } else {
    x = 2;
    y = 0;
  }

  assert((x + y) % 2 == 0);
}
