/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.*;
/**
 *
 * @author Pierre and Lilian Franchi
 */
public class Logger1 extends Logger{
    public Logger1(int lvl) {
        super(lvl);
    }

    @Override
    protected void writeMessage(String msg) {
        try{
            File f = new File("logger1.txt");
            if(!f.exists())
                f.createNewFile();
            try (FileWriter fw = new FileWriter(f)) {
                fw.write(msg + "\n");
            }
        } catch (Exception e) {
            System.out.println("Error message : " + e.getMessage());
        }
    }
}

