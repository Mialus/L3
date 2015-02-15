public class Carte implements Comparable {
	private Couleur couleur;
	private  Valeur valeur;

	
public Couleur getCouleur(){return couleur;}
	
public Valeur getValeur(){return valeur;}
	
public int getPoint(){
		switch (valeur){
			case SEPT:
			case HUIT:
			case NEUF:
				return 0;
			case DIX:
				return 5;
			case VALET:
				return 6;
			case DAME:
				return 7;
			case ROI:
				return 8;
			case AS:
				return 9;
			default:
				return 0;
		
		}
	}
	public Carte(Couleur c, Valeur v){
		couleur=c;
		valeur=v;
	}

	public String toString(){
	
	return valeur+" "+couleur;
	}

	public int compareTo(Object o){
		if(o instanceof Carte){
			if(this.getCouleur().ordinal()< ((Carte) o).getCouleur().ordinal())
				return -1;

			else if (this.getCouleur().ordinal()> ((Carte) o).getCouleur().ordinal())
				return 1;
	
			else if (this.getCouleur().ordinal()< ((Carte) o).getCouleur().ordinal())
				return -1;

			else if (this.getCouleur().ordinal()> ((Carte) o).getCouleur().ordinal())
				return 1;

			else
				return 0;

			}else {return 1;}
	}
}
