import java.util.*;

public class PaquetJoueur{

	ArrayList<TreeSet<Carte>> paquet;
	private int points;

	public PaquetJoueur(){
		points=0;
		paquet=new ArrayList<TreeSet<Carte>>();
		for(int i=0;i<Couleur.values().length;i++){
			paquet.add(new TreeSet<Carte>());
		}
}

public void addPoints(int p){
	points+=p;
}

public int getPoints(){return points;}

public int evaluePaquet(){
	int cpt=0;
	Iterator<TreeSet<Carte>> i = paquet.iterator();
	while(i.hasNext()){
	TreeSet<Carte> t=i.next();
	Iterator<Carte> it = t.iterator();
	while(it.hasNext()){
		cpt+=it.next().getPoint();
	}
}
return cpt;
}

public void ajouter(Carte c){
	paquet.get(c.getCouleur().ordinal()).add(c);
}

public void remove(Carte c){
	paquet.get(c.getCouleur().ordinal()).remove(c);
}
}
