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
public class Interpreteur2 implements ExprVisiteur2{
    
    @Override
    public int visit(Repertoire r){
        int val=0;
        if(!r.enf.isEmpty()){
        for(Noeud enf1 : r.enf){
            if(enf1 instanceof Repertoire){
                Repertoire rep = (Repertoire) enf1;
                val += visit(rep);
            }else{
                Fichier fic = (Fichier) enf1;
                val += visit(fic);
            }
        }
        }
        return val;
    }
    
    @Override
    public int visit(Fichier f) {
        int val=f.getTaille();
        return val;
    }
    
}
