/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author Pierre and Lilian Franchi
 */
public abstract class Logger {
    public static final int SUCCES=1, ACTIVE=2, SPEC=3, NONDETER=4;
    
    protected int level;
    protected Logger next;
    
    protected Logger(int lvl){
        level = lvl;
        next = null;
    }
    
    public Logger setNext(Logger l){
        next = l;
        return l;
    }
    
    public void message(String msg){
        if (next != null)
            next.message(msg);
        else
            writeMessage(msg);
    }
    
    protected abstract void writeMessage(String msg);
}

