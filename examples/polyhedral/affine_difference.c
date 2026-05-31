/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = rand(0, 5);
  int j = rand(0, 5);
  int x = i - j;
  assert(x >= -5);
  assert(x <= 5);
  assert(x != -5); //@KO
  assert(x != 5); //@KO
}
