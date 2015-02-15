#include "automate.h"

//Fonction qui ajoute une Transition
void ajouteTransition (automate *A,int state,int statefin, char eti)
{
ajouteListe(&A->trans[state][(int)(eti-97)],statefin);
}

//Fonction qui construit un automate
automate* construitAutomateExemple()
{
int i =0 ,j=0;
automate* A = malloc(sizeof(automate));
A->size = 5;
A->sizealpha = 2;
A->initial = (int*)malloc (A->size * sizeof(int));

for (i = 0; i < A->size ; i++) 
{ 
A->initial[i] = 0;
}

A->initial[0] = 1;
A->initial[1] = 1;
A->final = (int*)malloc (A->size * sizeof(int));

for (i = 0; i < A->size ; i++) 
{
A->final[i] = 0;
}

A->final[1] = 1;
A->final[4] = 1;
A->trans = (liste***) malloc (A->size * sizeof(liste**));

for (i = 0; i < A->size; i++) 
{
A->trans[i] = (liste**) malloc (A->sizealpha * sizeof(liste*));		
}

for(i = 0; i < A->size; i++)
{
	for(j = 0; j < A->sizealpha; j++)
	{
 	A->trans[i][j]=NULL;
 	}
 }

ajouteTransition(A,0,1,'a');
ajouteTransition(A,0,2,'a'); 
ajouteTransition(A,0,3,'a'); 
ajouteTransition(A,1,3,'b');
ajouteTransition(A,2,3,'a');
ajouteTransition(A,2,4,'b');
ajouteTransition(A,3,3,'b');
ajouteTransition(A,3,4,'b');
ajouteTransition(A,4,4,'a');
return A;
}

//construit un automate
automate* construitAutomateExempleProd1() 
{
int i =0 ,j=0;
automate* A = malloc(sizeof(automate));
A->size = 2;
A->sizealpha = 2;
A->initial = (int*)malloc (A->size * sizeof(int));

for (i = 0; i < A->size ; i++)
{
A->initial[i] = 0;
}
 
A->initial[0] = 1;
A->final = (int*)malloc (A->size * sizeof(int));
 
for (i = 0; i < A->size ; i++) { 
A->final[i] = 0;
}

A->final[1] = 1;
A->trans = (liste***) malloc (A->size * sizeof(liste**));
 
for (i = 0; i < A->size; i++) 
{
A->trans[i] = (liste**) malloc (A->sizealpha * sizeof(liste*));		
}

for(i = 0; i < A->size; i++)
{
 	for(j = 0; j < A->sizealpha; j++)
	{
 	A->trans[i][j]=NULL;
 	}
}
ajouteTransition(A,0,0,'a');
ajouteTransition(A,0,1,'b'); 
ajouteTransition(A,1,1,'a'); 
return A;
}

//construit un automate
automate* construitAutomateExempleProd2()
{
int i =0 ,j=0;
automate* A = malloc(sizeof(automate));
A->size = 3;
A->sizealpha = 2;
A->initial = (int*)malloc (A->size * sizeof(int));
 
for (i = 0; i < A->size ; i++) 
{
A->initial[i] = 0;
}
 
A->initial[0] = 1;
A->final = (int*)malloc (A->size * sizeof(int));
 
for (i = 0; i < A->size ; i++) 
{  
A->final[i] = 0;
}
 
A->final[2] = 1;
A->trans = (liste***) malloc (A->size * sizeof(liste**));

for (i = 0; i < A->size; i++) {
A->trans[i] = (liste**) malloc (A->sizealpha * sizeof(liste*));		
}

for(i = 0; i < A->size; i++)
{
 	for(j = 0; j < A->sizealpha; j++)
	{
	A->trans[i][j]=NULL;
 	}
}
ajouteTransition(A,0,0,'b');
ajouteTransition(A,0,1,'a'); 
ajouteTransition(A,1,1,'b'); 
ajouteTransition(A,1,2,'b'); 
ajouteTransition(A,2,2,'b'); 
return A;
}

//Fonction qui affiche l'automate
void afficheAutomate (automate *A)
{
int i = 0, j = 0; 
int cpt = 0;
liste * l = NULL;
if (A->trans != NULL)
{
printf("\nLes états initiaux : ");
	for(i = 0; i < A->size; i++)
	{
		if (A->initial[i] == 1)
		{
		cpt++;
		}
	}
printf("%d\n",cpt);
cpt = 0;
printf("Les états finaux : ");

for(i = 0; i < A->size; i++)
{
	if (A->final[i] == 1)
	{
	cpt++;
	}
}

printf("%d\n",cpt);
printf("Les transitions : \n");
	
for(i = 0; i < A->size; i++) 
{
printf("-----------------------\n");
printf("Depuis l'état %d :\n",i);

	for (j = 0 ; j < A->sizealpha ; j++)
	{
	printf("Avec la lettre %c :\n", j+'a'); 
	l = A->trans[i][j];
		if (l != NULL)
		{
		printf("Transition vers la ou les lettre(s) : ");				
			while (l != NULL)
			{
			printf("%d ",l -> state);
			l = l -> suiv;
			}
		printf("\n");
		}else
		{
		printf("Il n'y pas de transition avec la lettre %c !\n", j+'a');
		}
	}
}
}
}

//Fonction qui compte le nombre de transition
int compteTransitions (automate *A)
{
int i = 0, j = 0; // Itérateurs
int cpt = 0; // Compteur
liste * l = NULL;
 
for(i = 0; i < A->size; i++) 
{
 	for (j = 0 ; j < A->sizealpha ; j++)
	{
	l = A->trans[i][j];
 		while (l != NULL)
		{
		l = l -> suiv;
		cpt++;
		}
	}
}
 
return cpt;
}

//Fonction qui test si un automate est déterministe
int deterministe (automate *A)
{
int i = 0, j = 0;
int cpt = 0;
liste * l = NULL;

for (i=0; i < A->size; i++)
{
	if (A->initial[i] == 1)
	{
 	cpt++;
 	}
 	
	if(cpt > 1)
	{
 	return 0;
 	}
}

for(i = 0; i < A->size; i++) 
{
 	for (j = 0 ; j < A->sizealpha ; j++)
	{
 	l = A->trans[i][j];
 		if (l != NULL)
		{
			if(l -> suiv != NULL)
			{
			return 0;
			}
		}
 	}
}
return 1;
}

//Fonction qui test si un automate est complet ou non
int complet (automate *A)
{
int i = 0, j = 0;
liste * l = NULL;

for(i = 0; i < A->size; i++) 
{
 	for (j = 0 ; j < A->sizealpha ; j++)
	{
 	l = A->trans[i][j];
 		if (l == NULL)
		{
		return 0;
		}
 	}
}
return 1;
}

//Fonction qui test l'existance d'une transistion
int existeTransition (automate* A, int state, int statefin, char eti)
{
liste *tmp= A->trans[state][eti-'a'];

if(A->size-1 >= state) 
{
	 
	while(tmp != NULL)
	{
	 	if(tmp->state == statefin)
		{
		return 1;
		}else
		{
		tmp = tmp->suiv;
		}
	 }
}
return 0;
}

//Fonction qui supprime une transition
void supprimeTransition (automate* A, int state, int statefin, char eti)
{

 if(existeTransition(A,state,statefin,eti))
{
liste *tmp=NULL;
liste *l = A->trans[state][eti-'a'];

	if (l->state == statefin)
	{
	tmp = l ->suiv;
	free(A->trans[state][eti-'a']);
	A->trans[state][eti-'a'] = tmp;
	}else
	{
		while(l->suiv->state != statefin)
		{
		tmp = l;
		l = l ->suiv;
		}
	tmp = l -> suiv->suiv;
	free (l->suiv);
	l-> suiv = tmp;
	} 
   	//printf("\nLa transition a été supprimé\n");	
}
}

//Fonction qui test l'existance d'un état
int existeEtat (automate* A, int state)
{

if(A->size-1 < state) {
return 0;
}
return 1;
}


//Fonction qui supprime un état
void supprimeEtat (automate* A, int state)
{
int i =0, j=0;
liste * l = NULL;

if(existeEtat(A,state))
{
	 for (j = 0 ; j < A->sizealpha; j++)
	{
		for (i = 0; i < A->size; i++) 
		{
		supprimeTransition(A,state,i,j+'a');
		}
	}
 
 	for(i = 0; i < A->size; i++)
	{
		for (j = 0 ; j < A->sizealpha ; j++)
		{
		l = A->trans[i][j];
			while (l != NULL)
			{
				if(l->state > state)
				{
				l->state--;
				}
			l = l -> suiv;
			}
		}
	 }
free(A->trans[state]);
	 
	 for(i = 0; i < A->size-1; i++)
	 {
		if(i >= state && A->size > 1)
		{
		A->final[i] = A->final[i+1];
	 	A->initial[i] = A->initial[i+1];
		A->trans[i] = A->trans[i+1];
		}
	 }
	 
A->size--;
A->initial = (int*) realloc (A->initial, (A->size * sizeof(int)));
A->final = (int*) realloc (A->final, (A->size * sizeof(int)));
A->trans = (liste***) realloc(A->trans,(A->size * sizeof(liste**))); 

}
}

//Fonction qui complete un automate
void completeAutomate(automate *A)
{
int i = 0, j = 0;

A->size++;
A->trans = (liste***) realloc (A->trans,A->size * sizeof(liste**));
A->trans[A->size-1] = (liste**) malloc (A->sizealpha * sizeof(liste*));
A->initial = (int*) realloc (A->initial, (A->size * sizeof(int)));
A->final = (int*) realloc (A->final, (A->size * sizeof(int)));
A->initial[A->size-1] = 0;
A->final[A->size-1] = 0;

for(i = 0; i < A->sizealpha; i++)
{
A->trans[A->size-1][i]=NULL;
}
 	
for (i = 0; i < A->size; i++) 
{
	 for (j = 0 ; j < A->sizealpha; j++)
	 {
		if(A->trans[i][j]== NULL)
		{
		ajouteTransition(A,i,A->size-1,j+'a');
		}
	}
}
}

//Fonction qui fusionne deux états
void fusionEtats(automate *A, int state1, int state2)
{
int i = 0, j = 0;
 
if(A->initial[state1] == 0)
{
A->initial[state1] = A->initial[state2];
}

if(A->final[state1] == 0)
{
A->final[state1] = A->final[state2];
}

for (i = 0; i < A->size; i++) 
{
	 for (j = 0 ; j < A->sizealpha; j++)
	 {
		if(existeTransition(A,state2,i,j+'a'))
		{
			if(i == state2)
			{
			ajouteTransition(A,state1,i-1,j+'a');
			}else
			{
			 ajouteTransition(A,state1,i,j+'a');
			}
		}
		if(existeTransition(A,i,state2,j+'a'))
		{
		ajouteTransition(A,i,state1,j+'a');
		supprimeTransition(A,i,state2,j+'a');
		}
	}
}
supprimeEtat(A,state2);
printf("\nFusion des états %d et %d\n\n",state1,state2);
}

//Fonction qui test si lelangage est vide
int langageVide(automate* A)
{

if(compteTransitions(A)==0)
{
return 1;
}

graphe* graph = creerGraphe();
automateToGraphe(A,graph);
int i,j;
 
for(i=0;i<A->size;i++)
{
        for(j=0;j<A->size;j++)
	{
                if(A->initial[i] == 1 && A->final[j] == 1)
		{
                        if(chemin(graph,i,j) == 1)
			{
                        return 0;
                        }
                }
        }
}
return 1;
}

//Fonction qui supprime les états Non co-accessible
void supprimeNonCoAccessibles(automate* A)
{
int i,j;
int tab[A->size];
int cpt = 0;
 
for(i=0;i<A->size;i++)
{
tab[i] = 0;
}
        
graphe* graph = creerGraphe();
automateToGraphe(A,graph);
        
for(i=0;i<A->size;i++)
{
        for(j=0;j<A->size;j++)
	{
		if(A->final[j] == 1 && chemin(graph,i,j) == 1)
		{                 				
		tab[i] = 1;
		}
        }
}
 
do 
{
	for(i=0;i<A->size;i++)
	{
	 	if(tab[i] != 1)
		{
	      	supprimeEtat(A,i);
	      	printf("Suppression de l'état : %d car non coaccessible.\n",i);	
	      		if(tab[i+1] == 1)
			{
      			tab[i] = 1;
	      		}				
	      	}
	 }
cpt++;
}while(cpt != 5);
}

//Fonction qui supprime les états non-accessible
void supprimeNonAccessibles(automate* A)
{
int i,j;
int tab[A->size];
int cpt = 0;
 
for(i=0;i<A->size;i++)
{
tab[i] = 0;
}
        
graphe* graph = creerGraphe();
automateToGraphe(A,graph);
        
for(i=0;i<A->size;i++)
{
        for(j=0;j<A->size;j++)
	{
		if(A->initial[i] == 1 && chemin(graph,i,j) == 1)
		{                 				
		tab[j] = 1;
		}
        }
}

do
{ 
	for(i=0;i<A->size;i++)
	{
	 	if(tab[i] != 1)
		{
	      	supprimeEtat(A,i);
	      	printf("Suppression de l'état : %d car non accessible.\n",i);	
	      		if(tab[i+1] == 1)
			{
	      			tab[i] = 1;
	      		}				
	      	}
	}
cpt++;
}while(cpt != 5);
}

//Fonction qui fait le produit d'automate
void produit(automate* autoP1, automate* autoP2, automate* prod)
{
int size,i,j;
int taille, sizeAlpha;
automate* petit;
automate* grand;
liste* tmpPetit;
liste* tmpGrand;

if(autoP1->size >= autoP2->size)
{
size = autoP1->size;
grand = autoP1;
petit = autoP2;
}else
{
size = autoP2->size;
grand = autoP2;
petit = autoP1;
}

taille = size * petit->size;

if(autoP1->sizealpha >= autoP2->sizealpha)
{
sizeAlpha = autoP2->sizealpha;
}else
{
sizeAlpha = autoP1->sizealpha;
}

prod->size = taille;
prod->sizealpha = sizeAlpha;
prod->initial=(int*) malloc(taille*sizeof(int));
prod->final=(int*) malloc(taille*sizeof(int));

for(i=0; i<taille; i++)
{
        if(petit->initial[i/size] == 1 && grand->initial[i%size] == 1)
	{
        prod->initial[i] = 1;
        }else
	{
        prod->initial[i] = 0;
        }
        if(petit->final[i/size] == 1 && grand->final[i%size] == 1)
	{
        prod->final[i] = 1;
        }else
	{
        prod->final[i] = 0;
        }
}

prod->trans=(liste***) malloc(taille*sizeof(liste**));
for(i=0;i<taille;i++)
{
prod->trans[i]=(liste**) malloc(sizeAlpha*sizeof(liste*));
        for(j=0;j<sizeAlpha;j++)
	{
        prod->trans[i][j]=NULL;
        }
}

for(i=0; i<taille; i++)
{
        for(j=0; j<sizeAlpha; j++)
	{
        tmpGrand = grand->trans[i%size][j] ;
        tmpPetit = petit->trans[i/size][j] ;

        	while(tmpPetit != NULL)
		{                        
                        while(tmpGrand != NULL)
			{      
                        ajouteTransition(prod,i,((tmpPetit->state * size)+tmpGrand->state),(char)('a' + j));
                        tmpGrand = tmpGrand->suiv;
                        }
                tmpPetit = tmpPetit->suiv;
                tmpGrand = grand->trans[i%size][j] ;
                }
        }
}
printf("\n-----Automate Prod------\n");
afficheAutomate(prod);
}

graphe* automateToGraphe(automate* A, graphe* graph){
 graph->nbSommet = A->size;
 int n = graph->nbSommet;
 int i,j;
 for(i=0;i<n;i++){
        ajouteSommet(graph);
 }
 for(i=0;i<A->size;i++){
        for(j=0;j<A->sizealpha;j++){
                liste* tmp = A->trans[i][j];
                while(tmp != NULL){
                        ajouteArete(graph,i,tmp->state);
                        tmp = tmp->suiv;
                }
        }
 }
return graph;
}

//Fonction qui libére la mémoire
void mem(automate *A)
{
int i=0;
int tmpSize=A->size;
int nb=0;
 
for (i = 0 ; i < tmpSize; i++)
{
supprimeEtat(A,0);
}
 
free(A->trans);
free(A->initial); 
free(A->final);
free(A);
}
