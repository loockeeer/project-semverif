/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(-2, 3);
  int j = rand(4, 8);
  int x = i + j;
  assert(x >= 2);
  assert(x <= 11);
  assert(x != 2); //@KO
  assert(x != 11); //@KO
}
