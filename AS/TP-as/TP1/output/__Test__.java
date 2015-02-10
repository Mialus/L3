import java.io.*;
import org.antlr.runtime.*;
import org.antlr.runtime.debug.DebugEventSocketProxy;


public class __Test__ {

    public static void main(String args[]) throws Exception {
        tplexerLexer lex = new tplexerLexer(new ANTLRFileStream("/afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/output/__Test___input.txt", "UTF8"));
        CommonTokenStream tokens = new CommonTokenStream(lex);

        tplexerParser g = new tplexerParser(tokens, 49100, null);
        try {
            g.LETTER();
        } catch (RecognitionException e) {
            e.printStackTrace();
        }
    }
}