#ifndef __CBIGCHARACTERPAINTERSPACE_H
#define __CBIGCHARACTERPAINTERSPACE_H

class CBigCharacterPainterSpace : public CBigCharacterPainter
{
public:

	explicit				CBigCharacterPainterSpace	(void);
	virtual					~CBigCharacterPainterSpace	(void);

	virtual int				GetLineCount				(void);
	virtual const char *	GetLine						(int nLine);

protected:

private:

};

#endif // __CBIGCHARACTERPAINTERSPACE_H

