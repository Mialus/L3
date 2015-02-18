class Garniture extends Pizza {

Pizza originale;
String[] ingre;


public void choixIngre(Pizza p, String[] ing, int nbIngre){
originale=p;
ingre=new String[nbIngre];
ingre=ing;
}

public void getDescription(){
if(ingre.length==0){
System.out.println("Pâtes");
}else{
for(int i=0;i<ingre.length;i++){
if(i==0){
System.out.println("Pâtes+");
}
if(i!=ingre.length-1){
System.out.println(ingre[i]+"+");
}
if(i==ingre.length-1){
System.out.println(ingre[i]);
}

}
}
}
}
