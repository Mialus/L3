#ifndef Liste_H
#define Liste_H
#include <stdlib.h>
#include <stdio.h>

typedef struct s_liste {
        int state;
        struct s_liste* suiv;
} liste;

void ajouteListe(liste** l, int q);
void afficheListe(liste* l);
#endif
