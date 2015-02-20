/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mcootp1;

/**
 *
 * @author Gabriel
 */
import java.util.*;

public class Repertoire extends Noeud implements Expr{
public List<Noeud> enf = new ArrayList<>();

public Repertoire(String nom){
m_nom=nom;
m_taille=4096;
}

public void delNoeud(int index){

	enf.remove(index);
}

public void addNoeud(Noeud n){

	enf.add(n);
        n.parent=this;
}

    @Override
    public void accept(ExprVisiteur v) {
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

