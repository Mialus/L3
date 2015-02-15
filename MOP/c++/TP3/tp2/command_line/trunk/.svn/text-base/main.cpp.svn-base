//------------------------------------------------------------------------------
// TP MOPO #2 / Travail collaboratif en utilisant des branches
//------------------------------------------------------------------------------

#include "headers.h"

#define LETTER_MAX 256

int main()
{
	const char * szMessage = "VOICI UN PANGRAMME CONNU : PORTEZ CE VIEUX WHISKY AU JUGE BLOND QUI FUME";

	CBigCharacterPainter * ppBigCharacterPainter[LETTER_MAX];

	CBigCharacterPainterDefault * pDefaultCharacterPainter = new CBigCharacterPainterDefault();

	//
	// Ajout d'une instance de classe par défaut pour chaque caractère (espace)
	for (int nLetter = 0 ; nLetter < LETTER_MAX ; ++nLetter)
	{
		ppBigCharacterPainter[nLetter] = NULL;
	}

	//
	// Ajout des instances des classes liées à chaque caractère

	//
	// Affichage du message
	for (unsigned int nChar = 0 ; nChar < strlen(szMessage) ; ++nChar)
	{
		CBigCharacterPainter * pBigCharacterPainter = ppBigCharacterPainter[szMessage[nChar]];
		if (NULL == pBigCharacterPainter)
		{
			pBigCharacterPainter = pDefaultCharacterPainter;
		}

		for (int nLine = 0 ; nLine < pBigCharacterPainter->GetLineCount() ; ++nLine)
		{
			printf("%s\n", pBigCharacterPainter->GetLine(nLine));
			usleep(40000);
		}

		printf("\n");
	}

	//
	// Libération de la mémoire allouée
	for (int nLetter = 0 ; nLetter < LETTER_MAX ; ++nLetter)
	{
		if (NULL != ppBigCharacterPainter[nLetter])
		{
			delete ppBigCharacterPainter[nLetter];
			ppBigCharacterPainter[nLetter] = NULL;
		}
	}

	if (NULL != pDefaultCharacterPainter)
	{
		delete pDefaultCharacterPainter;
		pDefaultCharacterPainter = NULL;
	}
}
