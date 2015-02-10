import java.til.*;

public abstract class Chambre{
	private int nbLits;
	private int etage;
	private int numero;
	private boolean reserve;
	private float tarifDeBase; //tarif de base
	
	final static int NB_MAX_ETAGE = 9; //creation d une constante pour le nombre d etage
	final static float TARIF_LIT=10.0; //constante pour un supplement de lit
	
	public Chambre(int n1, int e, int num, int t) //methode chambre (constructeur)
	{
		nblits =n1;
		etage=e;
		numero=num;
		tarifDeBase=t;
		
		
		if (etage <= 0 || numero >999) 
		{//lever l exception et choisir une exception suffisament parlant, pour retourner une erreur
			throws new IndexOutOfBoundsException("Erreur, le numero de la chambre n'est pas compris entre 0 et 999");	
		}
		else if (etage<1 || etage > NB_MAX_ETAGE)
		{
			throws new IndexOutOfBoundsException("Erreur, le numero de la chambre n'est pas compris entre 1 et le nombre max d'etage");
			
		}
		else if (etage!= numero/100)
		{
			throws new IndexOutOfBoundsException("Erreur, le numero de la chambre n'est pas egale au nombre de chambre/100");
		}
		
	}
	
	public int getLits(){
		return nbLits;
	}
	
	public int getEtage(){
		return etage;
	}
	
	public int getNumero(){
		return numero;
	}
	
	public int getTarif(){ 
		return tarifDeBase*TARIF_LIT*nbLits;
	}
	
	public void Reserver() {return reserve;}

	
	public String toString(){
		String texte =" [ "+numero+":"nbLits+":"+Lits+" "(s) ] ";
		if (reserve)
			return texte;
		else
		

	}
}