import java.til.*;

public abstract class ChambreLuxe extends ChambreSimple{ //classe ChambreSimple herite de la classe Chambre

	final static float TARIF_MINIBAR=30.0;
	final static flaot MAJORATION=5.5;
	boolean miniBar;
	
	public ChmabreLuxe(int n1, int e, int num, int t, boolean b){
		super(n1,e, num TARIF_CHAMBRE_SIMPLE); //appel la methode de la classe parent
		miniBar=d; 
	}
	
	public boolean getMiniBar ()
	{
		return miniBar;
	}
	
	public boolean getTarif ()
	{
		if(miniBar)
			return super.getTarif()* MAJORATION + MINIBAR_price;
		else 
			return super.getTarif
	}
}