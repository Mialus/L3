#include "graphe.h"

//Fonction qui affiche un graphe
void afficheGraphe(graphe* graph)
{
liste_adj* tmp = graph->trans;
printf("\n------Graphe------\n");

while(tmp != NULL)
{
printf("\nPour l'etat %d\n",tmp->state);
liste* tmp2 = tmp->voisins;

        while(tmp2 != NULL)
	{
        printf("%d ",tmp2->state);
        tmp2 = tmp2->suiv;
        }

tmp = tmp->suiv;
}
printf("\n");
}

//Fonction qui ajoute un sommet 
void ajouteSommet(graphe* graph)
{
liste_adj* newListeAdj = (liste_adj*) malloc(sizeof(liste_adj));
liste_adj* tmp = graph->trans;

if(tmp == NULL)
{
newListeAdj->state = 0;
newListeAdj->voisins = NULL;
newListeAdj->suiv = NULL;
graph->nbSommet = 1;
graph->trans = newListeAdj;
return;
}

while(tmp->suiv != NULL)
{
tmp = tmp->suiv;
}

newListeAdj->state = graph->nbSommet;
newListeAdj->voisins = NULL;
newListeAdj->suiv = NULL;
graph->nbSommet++;
tmp->suiv = newListeAdj;
}

//Fonction qui ajoute une arête à un graphe
void ajouteArete(graphe* graph, int depart, int arrivee)
{
liste_adj* tmp = graph->trans;
 
if((depart >= (graph->nbSommet)) || (depart < 0))
{
return;
}
 
while(tmp->state != depart)
{
tmp = tmp->suiv;
}
ajouteListe(&(tmp->voisins),arrivee);
}

//Fonction qui creer un graphe
graphe* creerGraphe()
{
graphe* res = (graphe*) malloc(sizeof(graphe));
res->nbSommet = 0;
res->trans = NULL;
return res;
}
//Fonction qui permet d'obtenir la valeur d'un état d'un graphe
getvaleur(graphe* graph, int state)
{

if((state > graph->nbSommet - 1) || (state < 0))
{
return -1;
}

liste_adj* tmp = graph->trans;
 
while(tmp->state != state)
{
tmp = tmp->suiv;
}
 
return tmp->valeur;
}


//Fonction qui permet de modifier la Valeur d'un état d'un graphe
void setvaleur(graphe* graph, int state, int valeur)
{ 

if(valeur == -1)
{
printf("\nerreur : setValeur");
return;
}

if((state > graph->nbSommet - 1) || (state < 0))
{
printf("\nerreur : setValeur");
return;
}
 
liste_adj* tmp = graph->trans;
 
while(tmp->state != state)
{
tmp = tmp->suiv;
}

tmp->valeur = valeur;
return;
}

//Fonction qui permet de parcourir un graphe
void parcourGraphe(graphe* graph, int state)
{

if((state >= graph->nbSommet) || (state < 0))
{
return;
}

setvaleur(graph,state,1);
liste_adj* tmp = graph->trans;

while(tmp->state != state)
{
tmp = tmp->suiv;
}
liste* tmp2 = tmp->voisins;
 
while(tmp2 != NULL)
{
        if(getvaleur(graph,tmp2->state) == 0)
	{
        parcourGraphe(graph,tmp2->state);
        }
tmp2 = tmp2->suiv;
}
}

//Fonction qui test l'existence d'un chemin
int chemin(graphe* graph, int p, int q)
{
int i;
 
for(i=0;i<graph->nbSommet;i++)
{
setvaleur(graph,i,0);
}
 
parcourGraphe(graph,p);
return (getvaleur(graph,q));
}
