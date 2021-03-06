/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import static java.sql.Types.NULL;

/**
 *
 * @author Gabriel
 */
public class Interpreteur implements ExprVisiteur{
    
    public void visit(Repertoire r){
        if(r.getParent()!=null){
            testPar(r.getParent());
            System.out.print("|__");
        }
        if(r.enf.isEmpty()){
        System.out.println(r.m_nom);
        }else{
        System.out.println(r.m_nom);
        for(Noeud enf1 : r.enf){
    	 enf1.accept(this);
        }
        }
    }

    public void testPar(Repertoire re){
        
        if(re.getParent()!=null){
	    System.out.print("|  ");
            testPar(re.getParent());
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
