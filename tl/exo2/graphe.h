#ifndef Graphe_H
#define Graphe_H
#include <stdlib.h>
#include <stdio.h>
#include "liste.h"

typedef struct liste_adjacence{
        int state;
        int valeur;
        struct liste_adjacence* suiv;
        liste* voisins;
}liste_adj;

typedef struct {
        int nbSommet;
        liste_adj* trans;
}graphe;

int chemin(graphe* graph, int p, int q);
void parcourGraphe(graphe* graph, int state);
void afficheGraphe(graphe* graph);
graphe* creerGraphe();
void setvaleur(graphe* graph, int state, int valeur);
void ajouteSommet(graphe* graphe);
void ajouteArete(graphe* graph, int state1, int state2);
#endif
