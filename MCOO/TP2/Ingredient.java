class Ingredient extends Garniture{

Garniture ori;
String ingre;

	public Ingredient(Pizza p, String nom){
	originale = p;
	ingre=nom;
	}

	public void getDescription(){
	System.out.println(ingre);
	}
}
