/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mcootp1;

import static java.sql.Types.NULL;

/**
 *
 * @author Gabriel
 */
public class Interpreteur implements ExprVisiteur{
    
    public void visit(Repertoire r){
        if(r.getParent()!=null){
            System.out.print("|__");
        }
        if(r.enf.isEmpty()){
        System.out.println(r.m_nom);
        }else{
        System.out.println(r.m_nom);
        for(Noeud enf1 : r.enf){
            if(enf1 instanceof Repertoire){
                Repertoire rep = (Repertoire) enf1;
                visit(rep);
            }else{
                Fichier fic = (Fichier) enf1;
                visit(fic);
            }
        }
        }
    }

    public void testPar(Repertoire re){
        
    System.out.print("|  ");
        if(re.getParent()!=null){
            testPar(re);
        }
    }
    public void visit(Fichier f) {
        if(f.getParent()!=null){
            testPar(f.getParent());
            System.out.print("|__");
        }
        System.out.println(f.m_nom);
    }
}
