import java.util.*;


public abstract class Noeud
{
    protected String nom;
    protected int taille;
    protected Noeud pere;

    public String getNom()
    {
        return nom;
    }
    
    public int getTaille()
    {
        return taille;
    }
    
    public void setNom(String n)
    {
        nom = n;
    }
    
    
    
}

