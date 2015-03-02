/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import java.util.*;
/**
 *
 * @author Pierre
 */
public class Automate extends Observable{
    public Etat m_courant;
    public Etat m_ssAuto;
    public List<Transition> m_trans;
    public List<Etat> m_etat;
    
    public Automate(){
        m_trans = new ArrayList<>();
        m_etat = new ArrayList<>();
    }
    
    public Etat getCurrentState(){
        return m_courant; 
    }
    
    public void capture(Label l){
        if(l.m_trans.etatD==m_courant){//si l'état de départ de la transistion du label est
            m_courant=l.m_trans.etatA;//l'état courant, alors l'état courant devient l'état d'arrivé de la transtion
        }
        
    }
    public void initialiser(){
        for (int i=0;i<m_etat.size();i++){
            if(m_etat.get(i).m_initial){
            m_courant=m_etat.get(i);
            }
        }        
    }
    
    public void ajouteEtat(Etat e){
        int comp=0;
        if(!e.m_initial){// on test si e est un état initiale, si non, on l'ajoute directement
            ajouteEtat2(e);
        }else{
            for(int i=0;i<m_etat.size();i++){ // si oui, on test si l'automate à un état initiale
                if(!m_etat.get(i).m_initial){
                    comp++;
                }
            }
            if(comp==(m_etat.size()-1)){// si l'automate n'a pas d'état initiale, alors on lui met e
                ajouteEtat2(e);
            }
        }

    }
        
    public void ajouteEtat2(Etat e){
        m_etat.add(e);
        e.m_auto=this; 
    }
    
    public void ajouteTransition(Transition t){
        m_trans.add(t);
        t.m_auto=this;
    }
}
