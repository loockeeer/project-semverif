/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(0, 3);
  int j = rand(0, 3);
  int x = i * j;
  assert(x >= 0);
  assert(x <= 9);
  assert(x != 0); //@KO
  assert(x != 9); //@KO
}
