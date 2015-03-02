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
public class Etat {
    public String m_nom;
    public boolean m_initial;
    public boolean m_final;
    public Automate m_auto;
    public List<Transition> transPart = new ArrayList<>();
    public List<Transition> transArr = new ArrayList<>();
    public List<Automate> m_sousAuto = new ArrayList<>();

    public Etat(String nom, boolean initial, boolean finale){
        m_nom=nom;
        m_initial=initial;
        m_final=finale;
        
    }
    
    public void ajouterSousAutomate(Automate a){
        m_sousAuto.add(a);
        a.m_ssAuto=this;
    }
}
