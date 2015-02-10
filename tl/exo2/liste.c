#include "liste.h"

void afficheListe(liste* l){ 

  while (l != NULL){ 
      printf ("%d\t",l->state); 
      l = l->suiv; 
  } 
	printf("\n");
}

void ajouteListe(liste** l, int q)
{
	liste* ptl;
	liste* tmp;
	ptl=*l;
	if(!ptl)
	{
		ptl=(liste*) malloc(sizeof(liste));
		ptl->state=q;
		ptl->suiv=NULL;
		*l=ptl;
		return;
	}
	if(ptl->state == q)
	{ return;
	}
	if(q<ptl->state)
	{
		tmp=*l;
		*l=(liste*) malloc(sizeof(liste));
		(*l)->state=q;
		(*l)->suiv=tmp;
		return;
	}

	while(ptl->suiv && ptl->suiv->state<q)
	{
	ptl=ptl->suiv;
	}
	if(!ptl->suiv)
	{
		ptl->suiv=(liste*) malloc(sizeof(liste));
		ptl=ptl->suiv;
		ptl->state=q;
		ptl->suiv=NULL;
		return;
	}
	if(ptl->suiv->state==q){return;}
	tmp=ptl->suiv;
	ptl->suiv=(liste*) malloc(sizeof(liste));
	ptl=ptl->suiv;
	ptl->state=q;
	ptl->suiv=tmp;
}
