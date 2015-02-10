#include "CClient.h"
#include "CCompte.h"
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#define AJOUT 1
#define AJOUTCOMPTE 2
#define SUPPRESSION 3
#define LISTEPARTICULIERS 4
#define LISTEPROFESSIONNELS 5
#define COMPTESOLDE 6
#define NBCLIENT 7
#define NBCOMPTE 8
#define DEPOT 9
#define RETRAIT 10
#define TRANSFERT 11
#define QUIT 0
#define NBR 4
#define TAILLE 256


void AjoutClient(char pro)
{/*
char info[NBR][TAILLE];

switch(pro)
{
	case 'y':
	
	CProfessionnel client;

	printf("Veuillez donnez la raison sociale du client : \n");
	scanf("%c",&info[0]);
	printf("Veuillez donnez le SIRET du client : \n");
	scanf("%c",&info[1]);
	printf("Veuillez donnez l'adresse du client : \n");
	scanf("%c",&info[2]);
	printf("Veuillez donnez le numéro de téléphone du client : \n");
	scanf("%c",&info[3]);
	
	client.setRaiSocial(info[0]);
	client.setSIRET(info[1]);
	client.setAdresse([2]);
	client.setTelephone([3]);
	break;

	case 'n':

	CParticulier client;
	
	printf("Veuillez donnez le prénom du client : \n");
	scanf("%c",&info[0]);
	printf("Veuillez donnez le nom du client : \n");
	scanf("%c",&info[1]);
	printf("Veuillez donnez l'adresse du client : \n");
	scanf("%c",&info[2]);
	printf("Veuillez donnez le numéro de téléphone du client : \n");
	scanf("%c",&info[3]);

	client.setPrenom(info[0]);
	client.setNom[info[1]);
	client.setAdresse([2]);
	client.setTelephone([3]);
	
	break;

}*/

}


//Fonction qui supprime un client
void Suppression(CClient client)
{


}

//Fonction qui ajoute un compte d'épargne à un client
void AjoutCompte(CClient client)
{

}

//Fonction qui affiche la liste des clients particuliers
void ListeParticulier()
{

}

//Fonction qui affiche la liste des clients professionnels
void ListeProfessionnel()
{

}

//Fonction qui affiche les comptes d'un client et le solde associé
void CompteSolde(CClient client)
{

}

//Fonction qui affiche le nombre de client total
void NbClient()
{

}

//Fonction qui affiche le nombre de compte ouvert dans l'établissement
void NbCompte()
{

}

//Fonction qui permet de déposer des espèce sur le compte d'un client 
void Depot(CClient client,CCompte compte,float nbr)
{

}

//Fonction qui permet le retrait d'espèce que un compte
void Retrait(CClient client,CCompte compte,int nbr)
{


}
//Fonction qui permet de transférer des espèce entre deux comptes d'un même client
void Transfert(CClient client, CCompte compte,CCompte compte2, int nbr)
{

}



int main()
{
CClient client;
int nbr;
CCompte compte,compte2;
char info;
int choix;
 
do 
{
printf("Ajout d'un nouveau Client ? tapez 1 \n");
printf("Ajout d'un compte d'épargne pour un utilisateur existant ? tapez 2 \n");
printf("Suppression d'un client ? tapez 3 \n");
printf("Affichage de la liste des clients particulier ? tapez 4 \n");
printf("Affichage de la liste des clients proffessionnel ? tapez 5 \n");
printf("Affichage de la liste des comptes d'un client, ainsi que le solde associé ? tapez 6 \n");
printf("Affichage du nombre de clients total ? tapez 7 \n");
printf("Affichage du nombre total de comptes ouverts dans l'établissement ? tapez 8 \n");
printf("Dépot d'espèces sur le compte d'un client ? tapez 9 \n");
printf("Retrait d'espèces sur le compte d'un client ? tapez 10 \n");
printf("Transfert d'espèce entre deux comptes d'un même client ? tapez 11 \n");
printf("sinon, tapez 0 pour quitter\n");
scanf("%d",&choix);

switch(choix)
{

	case AJOUT:
		do
		{
		printf("est-ce un client professionnel ? (y/n) \n");
		scanf("%c",&info);
		}while((info!='y')&&(info=!'n'));

	AjoutClient(info);

	break;
	
	case AJOUTCOMPTE:
	AjoutCompte(client);

	break;

	case SUPPRESSION:
	Suppression(client);

	break;
	
	case LISTEPARTICULIERS:

	ListeParticulier();

	break;

	case LISTEPROFESSIONNELS:

	ListeProfessionnel();

	break;

	case COMPTESOLDE:
	CompteSolde(client);

	break;

	case NBCLIENT:

	NbClient();

	break;

	case NBCOMPTE:

	NbCompte();

	break;

	case DEPOT:

	Depot(client,compte,nbr);

	break;

	case RETRAIT:

	Retrait(client,compte,nbr);

	break;

	case TRANSFERT:

	Transfert(client,compte,compte2,nbr);

	break;

	case QUIT:
	return 0;

}
}while(QUIT==0);
}
