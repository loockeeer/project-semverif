/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(-2, 3);
  int j = rand(4, 8);
  int x = i + j;
  int y = i - j;
  assert(x + y == 2 * i);
  assert(x - y == 2 * j);
}
