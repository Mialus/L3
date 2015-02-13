public class Fichier extends Noeud{

public Fichier(String nom,int t,Repertoire r){
m_nom=nom;
m_taille=t;
parent=r;
}

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
