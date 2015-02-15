#ifndef Ctriangle_H
#define Ctriangle_H

class Ctriangle 
{

public:

	void setBase(float x);
	void setHauteur(float y);
	float getBase();
	float getHauteur();

protected:
 float m_base;
 float m_hauteur;
};
#endif
