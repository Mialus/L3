#ifndef CCourant_H
#define CCourant_H
#include "CCompte.h"

class CCourant : public CCompte
{

public:

	void setSolde(float sol);
	float getSolde();

protected:
 float sol;
};
#endif
