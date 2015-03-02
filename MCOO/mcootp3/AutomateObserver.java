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
public class AutomateObserver implements Observer{
    @Override
    public void update(Observable o, Object arg){
        if(o instanceof Automate){
            System.out.println(((Automate)o).m_courant.m_nom);
            Logger l = new Logger1(Logger.SUCCES);
            l.message("Etat actuelle : " + ((Automate)o).m_courant.m_nom);
        }
    }
}
