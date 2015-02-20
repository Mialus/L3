/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Gabriel
 */
public class FabriqueAVisiteur {
    
    public void afficheVisiteur(Noeud c){
        c.accept(new Interpreteur());
    }
    
    public int afficheVisiteur2(Noeud c){
        return c.accept2(new Interpreteur2());
    }
}
