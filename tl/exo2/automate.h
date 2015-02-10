#ifndef automate_H
#define automate_H
#include <stdlib.h>
#include <stdio.h>
#include "liste.h"
#include "graphe.h"

typedef struct {
         int size; 
         int sizealpha;
         int* initial;
         int* final;
         liste*** trans;
} automate;

void ajouteTransition (automate *A,int state,int statefin, char etiquette);
automate* construitAutomateExemple();
void afficheAutomate (automate *A);
void mem(automate *A);
int compteTransitions (automate *A);
int deterministe (automate *A);
int complet (automate *A);
graphe* automateToGraphe(automate* A, graphe* graph);
int existeTransition (automate* A, int state, int statefin, char etiquette);
void supprimeTransition (automate* A, int state, int statefin, char etiquette);
int existeEtat (automate* A, int state);
void supprimeEtat (automate* A, int state);
void completeAutomate(automate *A);
void fusionEtats(automate *A, int state1, int state2);
int langageVide(automate* A);
void supprimeNonCoAccessibles(automate* A);
void supprimeNonAccessibles(automate* A);
automate* construitAutomateExempleProd2();
automate* construitAutomateExempleProd2();
void produit(automate* autoP1, automate* autoP2, automate* prod);
#endif
