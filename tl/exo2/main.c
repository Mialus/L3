#include "automate.h"
#include "chaine.h"
int main () 
{
automate* A;
automate* prod;
graphe* graph = creerGraphe();
int nbTransition = 0;
int st,stfin,st2;
char eti;
int choix;

automate* AP1 = construitAutomateExempleProd1();
automate *AP2 = construitAutomateExempleProd2();
char* ch = malloc(12*sizeof(char));
char* ch2 = malloc(12*sizeof(char));
char* ch3 = malloc(24*sizeof(char));

A = construitAutomateExemple();
 
do {
printf("pour compter le nombre de transition de l'automate exemple, tapez 1 \n");
printf("pour tester si l'automate est déterministe, tapez 2 \n");
printf("pour savoir si l'automate est complet, tapez 3 \n");
printf("pour ajouter une transition à l'automate, tapez 4 \n");
printf("pour afficher l'automate, tapez 5 (l'exemple par défaut) \n");
printf("Pour utiliser les deux fonctions concat, tapez 6 \n");
printf("pour supprimer un état, tapez 7 \n");
printf("pour supprimer une transition, tapez 8 \n");
printf("pour completer un automate, tapez 9 \n");
printf("pour fusionner deux états, tapez 10 \n");
printf("pour savoir si il existe un chemin entre deux états, tapez 11 \n");
printf("pour créer le graphe associé à l'automate, tapez 12 \n");
printf("pour savoir si le langage est vide ou non, tapez 13 \n");
printf("pour supprimer les états qui ne sont pas co-accessible, tapez 14 \n");
printf("pour supprimer les états qui ne sont pas accessible, tapez 15 \n");
printf("pour faire le produit de deux automates, tapez 16 \n");
printf("pour compléter un automate, tapez 17 \n");
printf("pour afficher le graphe, tapez 18 (l'exemple par défaut) \n");
printf("sinon, tapez 0 \n");
scanf("%d",&choix);

switch(choix){

	case 1:

	nbTransition = compteTransitions(A);
	printf("Il y a %d transitions\n",nbTransition);


	break;

	case 2:

		if (!deterministe(A))
		{
		printf("L'automate n'est pas déterministe\n");
		}else{
		printf("L'automate est déterministe\n");
		}

	break;

	case 3:
	
		if (!complet(A))
		{
		printf("\nL'automate n'est pas complet !\n");
		}else
		{
		printf("\nL'automate est complet !\n");
		}
	break;

	case 4:
	printf("veuillez donner l'état de départ :\n");
	scanf("%d",&st);
	printf("veuillez donner l'état d'arrivée :\n");
	scanf("%d",&stfin);
	printf("veuillez donner la valeur de la transition :\n");
	scanf("%c",&eti);
	scanf("%c",&eti);// important sinon erreur de segmentation à cause du scanf
	ajouteTransition (A,st,stfin,eti);
	printf("\nfait!\n");

	break;

	case 5:
	afficheAutomate(A);
	break;

	case 6:

	printf("Veuillez entrer la premiére chaine de caractere\n");
	scanf("%s",ch);
	printf("Veuillez entrer la deuxieme chaine de caractere\n");
	scanf("%s",ch2);

	ch3=concatAvecStringH(ch,ch2);
	printf("Avec concatAvecStringH : %s\n",ch3);
	ch3=concatSansStringH(ch,ch2);
	printf("Avec concatSansStringH : %s\n",ch3);
	break;

	case 7:
	printf("veuillez donner l'état à supprimer :\n");
	scanf("%d",&st);
	supprimeEtat (A,st);
	printf("\nfait!\n");
	break;

	case 8:
	printf("veuillez donner l'état de départ :\n");
	scanf("%d",&st);
	printf("veuillez donner l'état d'arrivée :\n");
	scanf("%d",&stfin);
	printf("veuillez donner la valeur de la transition :\n");
	scanf("%c",&eti);
	scanf("%c",&eti);//même chose que pour ajouter
	supprimeTransition (A,st,stfin,eti);
	printf("\nfait!\n");
	break;

	case 9:
	completeAutomate(A);
	printf("\nfait!\n");
	break;

	case 10:
	printf("veuillez donner le premier état :\n");
	scanf("%d",&st);
	printf("veuillez donner le second état :\n");
	scanf("%d",&st2);
	fusionEtats(A,st,st2);
	break;

	case 11:
	printf("veuillez donner le premier état :\n");
	scanf("%d",&st);
	printf("veuillez donner le second état :\n");
	scanf("%d",&st2);
		if(chemin(graph,st,st2))
		{
		printf("\nLe chemin existe\n");
		}else
		{
		printf("\nLe chemin n'existe pas\n");
		}
	break;

	case 12:
	automateToGraphe(A,graph);
	printf("\nfait!\n");
	break;

	case 13:

		if(langageVide(A))
		{	
		printf("\nLe langage reconnu est vide\n");
		}else
		{
		printf("\nLe langage reconnu est non vide\n");
		}
	break;

	case 14:

	supprimeNonCoAccessibles(A);
	printf("\nfait!\n");

	break;

	case 15:
	supprimeNonAccessibles(A);
	printf("\nfait!\n");
	break;

	case 16:
	produit(AP1,AP2,prod);
	printf("\nfait!\n");
	break;
	
	case 17:// NON
		if (!complet(A))
		{
		completeAutomate(A);
		printf("\nL'automate est complet !\n");
		}else
		{
		printf("\nL'automate est complet !\n");
		}
	break;

	case 18:

	afficheGraphe(graph);
	
	break;
	case 0:
	return 0;

}
}while(choix!=0);
 
mem(A);
mem(AP1); 
mem(AP2);
return 0;
}
