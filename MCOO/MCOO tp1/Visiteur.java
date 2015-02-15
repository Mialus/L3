import java.util.*;

public class Visiteur
{

    Noeud noeudAct;
    
    public Visiteur()
    {
        noeudAct = null;
    }
    
    public Noeud getNoeudAct()
    {
        return noeudAct;
    }
    /*
    public void setNoeudAct(Noeud n)
    {
        noeudAct = n;
    }
    */
    public void parcours(Repertoire r, int cpt)
    {
        int i;
        HashMap<String, Noeud> liste = r.getEnfants();
        Noeud parcours;
        String nomParcours;
        Set<String> nomsRep = liste.keySet();
        Iterator<String> iter = nomsRep.iterator();
        System.out.println(r.getNom());
        while(iter.hasNext())
        {
            nomParcours = iter.next();
            parcours = liste.get(nomParcours);
            for(i = 0; i<cpt; i++)
            System.out.println("|    ");
            System.out.println("|____");
            System.out.println(nomParcours);
            if(parcours instanceof Repertoire)
            {
                parcours((Repertoire)parcours, cpt++);
            } 
        }
        cpt--;
    }
}
