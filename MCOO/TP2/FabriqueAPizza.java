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
public class FabriqueAPizza {
    
     public Pizza creerPizza(ArrayList<String> ing){
     Pizza p=new Garniture(ing);
     
     return p;
     }
}
