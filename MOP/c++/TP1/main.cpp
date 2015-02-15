#include<stdio.h> 
#include<stdlib.h>
#include "Crectangle.h"
#include "Cpignon.h"


int main()
{
  int nbrfac,t;
  float j;

  printf("Veuillez donner le nombre de façade : ");
  scanf("%d",&nbrfac);

while(nbrfac>0)
{
  Crectangle r;

  printf("Pour la façade n°%d\n",nbrfac);
  printf("\nVeuillez donner la largeur de la façade : \n");
  scanf("%f",&j);
  r.setLargeur(j);
  printf("\nVeuillez donner la hauteur de la façade hors pignon : \n");
  scanf("%f",&j);
  r.setHaut(j);
  printf("\nPrésence ou non d'un pignon ? (0/1) \n");
  scanf("%d",&t);
  printf("%d",t);

  if(t==1){
  printf("\nVeuillez donner la hauteur d'un pignon : \n");
  scanf("%f",&j);
  printf("%f",j);
  }

  printf("\nVeuillez donner le nombre de fenêtre : \n");
  scanf("%f",&j);
  printf("%f",j);
  printf("\nVeuillez donner le nombre de portes : \n");
  scanf("%f",&j);
  printf("%f",j);
nbrfac--;
}

return 0;
}
