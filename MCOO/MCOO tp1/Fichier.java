import java.util.*;

public class Fichier extends Noeud
{
    
    public Fichier(String s, int t, Noeud n)
    {
        nom = s;
        taille = t;
        pere = n;
    }
    
    
    public void setTaille(int t)
    {
        taille = t;
    }
}
