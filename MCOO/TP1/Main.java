/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Gabriel
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        FabriqueAVisiteur f = new FabriqueAVisiteur();
        Repertoire rep1=new Repertoire("rep1");
        Repertoire rep2=new Repertoire("rep2");
        Repertoire rep3=new Repertoire("rep3");
        Repertoire rep4=new Repertoire("rep4");
        Fichier fich1=new Fichier("fich1",100,rep2);
        Fichier fich2=new Fichier("fich2",75,rep3);
        Fichier fich3=new Fichier("fich3",17,rep3);
        Fichier fich4=new Fichier("fich4",28,rep2);
        
        rep1.addNoeud(rep2);
	rep2.addNoeud(fich1);
	rep2.addNoeud(fich4);
        rep2.addNoeud(rep3);
	rep3.addNoeud(fich2);
	rep3.addNoeud(fich3);
        rep1.addNoeud(rep4);
        
        f.afficheVisiteur(rep1);
        System.out.println("");
        System.out.println(f.afficheVisiteur2(rep1));
    }
    
}
