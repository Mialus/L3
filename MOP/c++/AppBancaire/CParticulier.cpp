#include "CParticulier.h"

void CParticulier::setNom(char* val)
{
	nom=val;
};

void CParticulier::setPrenom(char* val2)
{
	pre=val2;
};

char* CParticulier::getNom()
{
	return nom;
};

char* CParticulier::getPrenom()
{
	return pre;
};
