#ifndef Crectangle_H
#define Crectangle_H

class Crectangle 
{

public:

	void setLargeur(float x);
	void setHaut(float y);
	float getLargeur();
	float getHaut();

protected:
 float m_largeur;
 float m_haut;
};
#endif
