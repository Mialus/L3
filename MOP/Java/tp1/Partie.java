import java.util.*;

public class Partie{

	private PaquetJoueur j1=new PaquetJoueur();
	private PaquetJoueur j2=new PaquetJoueur();

	Partie(){
		Jeu distribution=new Jeu();
		distribution.remplir();
		distribution.melanger();
		boolean b=true;
		Carte c;
		while((c=distribution.depiler())!=null){
			if(b)
				j1.ajouter(c);
			else
				j2.ajouter(c);
			b=!b;
		}
}


private Carte max(PaquetJoueur j){
	
	Carte c=new Carte(Couleur.TREFLE,Valeur.SEPT);

	Iterator<TreeSet<Carte>> i = j.paquet.iterator();
	while(i.hasNext()){
	TreeSet<Carte> t=i.next();
	Iterator<Carte> it = t.iterator();
	while(it.hasNext()){
		if(c.getPoint()<it.next().getPoint()){
		c=it.next();	
		}
	}
}
return c;
}

private Carte min(PaquetJoueur j){
	
	Carte c=new Carte(Couleur.TREFLE,Valeur.AS);

	Iterator<TreeSet<Carte>> i = j.paquet.iterator();
	while(i.hasNext()){
	TreeSet<Carte> t=i.next();
	Iterator<Carte> it = t.iterator();
	while(it.hasNext()){
		if(c.getPoint()>it.next().getPoint()){
		c=it.next();	
		}
	}
}
return c;
}

private Carte minCoul(PaquetJoueur j, Couleur Coul){
	
	Carte c=new Carte(Couleur.TREFLE,Valeur.AS);

	Iterator<TreeSet<Carte>> i = j.paquet.iterator();
	while(i.hasNext()){
	TreeSet<Carte> t=i.next();
	Iterator<Carte> it = t.iterator();
	while(it.hasNext()){
		if((c.getPoint()>it.next().getPoint())&&(c.getPoint()<it.next().getPoint())){
		c=it.next();	
		}
	}
}
return c;
}

private Carte maxCoul(PaquetJoueur j,Couleur coul){
	
	Carte c=new Carte(coul,Valeur.SEPT);

	Iterator<TreeSet<Carte>> i = j.paquet.iterator();
	while(i.hasNext()){
	TreeSet<Carte> t=i.next();
	Iterator<Carte> it = t.iterator();
	while(it.hasNext()){
		if((c.getPoint()<it.next().getPoint())&&(c.getCouleur()==it.next().getCouleur())){
		c=it.next();	
		}
	}
}
return c;
}

private boolean TestCouleur(PaquetJoueur j,Couleur coul){
	boolean existCoul=false;

	Iterator<TreeSet<Carte>> i = j.paquet.iterator();
	while(i.hasNext()){
	TreeSet<Carte> t=i.next();
	Iterator<Carte> it = t.iterator();
	while(it.hasNext()){
		if(coul==it.next().getCouleur()){
		existCoul=true;
		}
	}
}
return existCoul;
}

public void Pli(){
	Carte c1,c2;
	int point;

	c1=max(j1);
	if(TestCouleur(j2,c1.getCouleur())){
	c2=maxCoul(j2,c1.getCouleur());
		if(c2.getPoint()<=c1.getPoint()){
		c2=minCoul(j2,c1.getCouleur());
		j1.addPoints(c2.getPoint()+c1.getPoint());
		}
	j2.addPoints(c2.getPoint()+c1.getPoint());
	}else{
	c2=min(j2);
	j1.addPoints(c2.getPoint()+c1.getPoint());
	}
	
	j1.remove(c1);
	j2.remove(c2);
}
}
