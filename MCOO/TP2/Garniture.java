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
public class Garniture extends Pizza{
protected String last;
List<String> ing = new ArrayList<>();

public Garniture(String ingr){
    if (!ing.contains(ingr)) {
            ing.add(ingr);
            last=ingr;
    }
}

public Garniture(){
}

public Garniture(ArrayList<String> in){
    for (String in1 : in) {
        if (!ing.contains(in1)) {
            ing.add(in1);
            last=in1;
        }
    }
}

    public String getLast() {
        return last;
    }

@Override
public void getDescription(){
    if(ing.isEmpty()){
        System.out.println("Pâtes");
        }else{
            for(int i=0;i<ing.size();i++){
                if(i==0){
                    System.out.print("Pâtes+");
                }
                if(i!=ing.size()-1){
                    System.out.print(ing.get(i)+"+");
                }
                if(i==ing.size()-1){
                    System.out.println(ing.get(i));
                }

            }
        }
}

}
