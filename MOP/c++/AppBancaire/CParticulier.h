#ifndef CParticulier_H
#define CParticulier_H
#include "CClient.h"

class CParticulier : public CClient
{
public:
	void setNom(char* nom);
	void setPrenom(char* pre);
	char* getNom();
	char* getPrenom();

protected:
 char* nom;
 char* pre;

};
#endif
