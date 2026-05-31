/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int b = rand(0, 1);
  int x;

  if(b == 0) {
    x = 0;
  } else {
    x = 2;
  }

  assert(x % 2 == 0);
}
