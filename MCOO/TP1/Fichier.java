/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Gabriel
 */
public class Fichier extends Noeud implements Expr{

public Fichier(String nom,int t,Repertoire r){
m_nom=nom;
m_taille=t;
parent=r;
}

    public int getTaille() {
        return m_taille;
    }

public void setNom(String nom){
m_nom=nom;
}

public void setTaille(int t){
m_taille=t;
}

public void accept(ExprVisiteur v){
	v.visit(this);
}

public Repertoire getParent() {
        return parent;
    }

    @Override
    public int accept2(ExprVisiteur2 ev) {
               return ev.visit(this);
    }
}

