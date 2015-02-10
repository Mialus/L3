#ifndef CEpargne_H
#define CEpargne_H
#include "CCompte.h"

class CEpargne : public CCompte
{

public:

	void setSolde(float sol);
	void setTaux(float tau);
	float getSolde();
	float getTaux();

protected:
 float sol;
 float tau;
};
#endif
