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

public class Transition {
    public Etat etatD;
    public Etat etatA;
    public Automate m_auto;
    public List<Label> e = new ArrayList<>();
    
    public Transition(Etat arr, Etat dep){
        etatA=arr;
        etatD=dep;
        etatA.transArr.add(this);
        etatD.transPart.add(this);
    }
}
