/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */	

/**
 *
 * @author Gabriel
 */
public interface Expr {
    void accept(ExprVisiteur ev);
    int accept2(ExprVisiteur2 ev);
    
}
