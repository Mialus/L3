#ifndef __CBIGCHARACTERPAINTER_H
#define __CBIGCHARACTERPAINTER_H

class CBigCharacterPainter
{
public:

	//
	// Construction / Destruction
	explicit				CBigCharacterPainter	(void);
	virtual					~CBigCharacterPainter	(void);

	//
	// Renvoie le nombre de lignes du caract�re
	virtual int				GetLineCount			(void) = 0;
	
	//
	// Renvoie la ligne n� nLine du caract�re
	virtual const char *	GetLine					(int nLine) = 0;

protected:
	
private:

};

#endif // __CBIGCHARACTERPAINTER_H

