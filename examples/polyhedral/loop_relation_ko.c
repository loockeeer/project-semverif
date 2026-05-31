/*
 * Cours "Sémantique et Application à la Vérification de programmes"
 *
 * Ecole normale supérieure, Paris, France / CNRS / INRIA
 */

void main(){
  int i = 0;
  int j = 0;
  while(rand(0, 1) == 0){
    i++;
    j += 2;
  }
  assert(j == 2 * i);
  assert(j == 0); //@KO
}
