public class Fichier extends Noeud{

public void setNom(String nom){
m_nom=nom;
}

public void setTaille(int t){
m_taille=t;
}

public Object accept(VisiteurNoeud v){

	return v;

}
}
