import java.util.*;

public class Repertoire extends Noeud{
private List<Noeud> enf = new ArrayList<>();
private int taille=4096;

public void setNom(String nom){
m_nom=nom;
}

public void delNoeud(int index){

	enf.remove(index);
}

public void addNoeud(Noeud n){

	enf.add(n);
}

public Object accept(VisiteurNoeud v){

	return v;

}

}
