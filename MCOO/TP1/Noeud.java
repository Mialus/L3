public abstract class Noeud{

protected String m_nom;
protected int m_taille;
protected Repertoire parent;


public void setNom(String nom){

}

public void setTaille(int t){
}

public abstract Object accept(VisiteurNoeud v);
}
