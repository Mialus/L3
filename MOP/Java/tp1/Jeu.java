import java.util.*;

public class Jeu {
	private List<Carte> ensemble = new LinkedList();

	public void remplir(){
		ensemble.clear();
	for(int v=0;v<=Valeur.values().length;v++){
		for(int c=0;c<=Couleur.values().length;c++){
			Carte carte = new Carte(Couleur.values()[c], Valeur.values()[v]);
			ensemble.add(carte);
		}
	}
}

	public String toString() {
		return ensemble.toString();
	}

	public void melanger(){Collections.shuffle(ensemble);}
	
	public Carte depiler(){
		Carte carte=null;
		if(ensemble.size()>0){
			carte=ensemble.get(0);
			ensemble.remove(0);
		}
	return carte;
	}
}
