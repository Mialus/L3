#include "CClient.h"

//Fonction qui permet l'enregistrement des informations d'un client en générale
void CClient::setAdresse(char* val)
{

	add=val;
}
void CClient::setTelephone(char* val2)
{

	tel=val2;

}

char* CClient::getAdresse()
{
return add;
}

char* CClient::getTelephone()
{
return tel;
}
