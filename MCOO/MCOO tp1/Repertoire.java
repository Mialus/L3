import java.util.*;

public class Repertoire extends Noeud
{
    
    protected HashMap<String, Noeud> enfants;
    
    
    public Repertoire(String n, Noeud p)
    {
        nom = n;
        taille = 4096;
        pere = p;
        enfants = new HashMap<String, Noeud>();
    }
    
    public void addNoeud(Noeud n)
    {
        enfants.put(n.getNom(), n);
    }
    
    public void delNoeud(String nom)
    {
        enfants.remove(nom);
    }
    
    public Noeud moveTo(String s)
    {
        return enfants.get(s);
    }
    
    public HashMap<String, Noeud> getEnfants()
    {
        return enfants;
    }
    
}
