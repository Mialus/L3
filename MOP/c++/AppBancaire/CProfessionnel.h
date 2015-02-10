#ifndef CProfessionnel_H
#define CProfessionel_H
#include "CClient.h"

class CProfessionnel : public CClient
{
public:
	void setRaiSocial(char* rai);
	void setSIRET(char* sir);
	char* getRaiSocial();
	char* getSIRET();

protected:
 char* rai;
 char* sir;

};
#endif
