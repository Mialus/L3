/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Gabriel
 */
public abstract class Noeud{

protected String m_nom;
protected int m_taille;
protected Repertoire parent;


public void setNom(String nom){

}

public void setTaille(int t){
}

public abstract Noeud getParent();
public abstract void accept(ExprVisiteur v);


public abstract int accept2(ExprVisiteur2 v);
}
