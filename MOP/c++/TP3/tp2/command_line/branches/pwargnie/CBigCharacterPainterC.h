#ifndef __CBIGCHARACTERPAINTERC_H
#define __CBIGCHARACTERPAINTERC_H

class CBigCharacterPainterC : public CBigCharacterPainter
{
public:

	//
	// Construction / Destruction
	explicit				CBigCharacterPainterC	(void);
	virtual					~CBigCharacterPainterC	(void);

	//
	// Renvoie le nombre de lignes du caract�re
	virtual int				GetLineCount			(void);
	
	//
	// Renvoie la ligne n� nLine du caract�re
	virtual const char *	GetLine					(int nLine);

protected:
	
private:

};

#endif
