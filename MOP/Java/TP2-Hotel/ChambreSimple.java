import java.til.*;

public abstract class ChambreSimple extends Chambre{ //classe ChambreSimple herite de la classe Chambre

	final static float TARIF_CHAMBRE_SIMPLE=30.0;
	final static flaot TARIF_DOUCHE=5.5;
	private boolean douche;
	
	public ChmabreSimple(int n1, int e, int num, int t, boolean d){
		super(n1,e, num TARIF_CHAMBRE_SIMPLE); //appel la methode de la classe parent
		douche=d; 
	}
	
	public boolean getDouche ()
	{
		return douche;
	}
	
	public float getTarif(){
		if(douche) 
			return super.gettarif()6+TARIF_DOUCHE;
		else
			return super.getTarif();
	}
	
	public String toString()[
		String texte=super.toString();
		if(douche)
			texte=texte.replace("lit(s)", "lit(s) (simple douche)");
		else
			texte=texte.replace("lit(s)", "lit(s) (simple)");
		return texte;
	}
	
	
		
	
	
}