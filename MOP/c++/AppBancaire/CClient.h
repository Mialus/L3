#ifndef CClient_H
#define CClient_H

class CClient
{

public:
	//Fonction qui permet l'enregistrement des informations d'un client en générale
	void setAdresse(char* add);
	void setTelephone(char* tel);
	char* getAdresse();
	char* getTelephone();

protected:
 char* add;
 char* tel;
};
#endif
