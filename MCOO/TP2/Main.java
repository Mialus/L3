/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
import java.util.*;
/**
 *
 * @author Gabriel
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        FabriqueAPizza f = new FabriqueAPizza();
        ArrayList<String> l1= new ArrayList<>();
        Pizza a,b;
        Olives o = new Olives();
        Anchois an = new Anchois();
        Champignon ch = new Champignon();
        Jambon j = new Jambon();
        Mozzarella mo = new Mozzarella();
        Oignon oi = new Oignon();
        Patate pa = new Patate();
        Tomates to = new Tomates();
        
        l1.add(o.getLast());
        l1.add(an.getLast());
        a= f.creerPizza(l1);
        a.getDescription();
        System.out.println("");
        l1.add(o.getLast());
        l1.add(an.getLast());
        l1.add(ch.getLast());
        l1.add(oi.getLast());
        b = f.creerPizza(l1);
        b.getDescription();
    }
    
}
