/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */


/**
 *
 * @author Pierre
 */
public class Label {
    public String m_etiquette;
    public Transition m_trans;
    
    public Label(Transition trans, String eti){
        m_etiquette=eti;
        m_trans=trans;
        m_trans.e.add(this);
    }
}
