/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Pierre
 */
public class Mcootp3 {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        Etat e1 = new Etat("e1", true, false);
        Etat e2 = new Etat("e2", false, false);
        Etat e3 = new Etat("e3", false, false);
        Transition t1 = new Transition(e1, e2);
        Label l1 = new Label(t1, "t1");
        Transition t2 = new Transition(e2, e3);
        Label l2 = new Label(t2, "t2");
        Transition t3 = new Transition(e3, e1);
        Label l3 = new Label(t3, "t3");
        
        Automate a1 = new Automate();
        a1.addObserver(new AutomateObserver());
        a1.ajouteEtat(e1);
        a1.ajouteEtat(e2);
        a1.ajouteEtat(e3);
        a1.initialiser();
        a1.ajouteTransition(t1);
        a1.ajouteTransition(t2);
        a1.ajouteTransition(t3);
        a1.capture(l1);
        a1.capture(l2);
        a1.capture(l3);
    }
    
}
