import java.util.*;

public class hotel extends ArrayList {
	private extends ArrayList listeChambre=new ArrayList();
	public ajouterChambre(chambre c) {
		if (!listeChambre.contains(c)) listeChambre.add(c);
		Collections.sort(listeChambre);
	}

	public Iterator<Chambre> getIterator(){
		return listeChambre.iterator();
		}
		public void Reserver() {return reserve;
		}
		public String toString() {
		Iterator<Chambre> i=new Iterator(listeChambre);
		while (i.hasNext()){ texte=texte+i.next()+"\n";
		}
		return texte;		
		}