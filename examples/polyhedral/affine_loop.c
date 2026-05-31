/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = 0;
  int j = 0;
  while(i != 5){
    i++;
    j++;
  }
  assert(i == 5);
  assert(j == 5);
  assert(i == j);
}
