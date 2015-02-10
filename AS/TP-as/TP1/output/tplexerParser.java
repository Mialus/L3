// $ANTLR 3.5 /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g 2015-01-20 14:13:12

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

import org.antlr.runtime.debug.*;
import java.io.IOException;
@SuppressWarnings("all")
public class tplexerParser extends DebugParser {
	public static final String[] tokenNames = new String[] {
		"<invalid>", "<EOR>", "<DOWN>", "<UP>", "LETTER", "WS"
	};
	public static final int EOF=-1;
	public static final int LETTER=4;
	public static final int WS=5;

	// delegates
	public Parser[] getDelegates() {
		return new Parser[] {};
	}

	// delegators


	public static final String[] ruleNames = new String[] {
		"invalidRule", "start"
	};

	public static final boolean[] decisionCanBacktrack = new boolean[] {
		false, // invalid decision
	};

 
	public int ruleLevel = 0;
	public int getRuleLevel() { return ruleLevel; }
	public void incRuleLevel() { ruleLevel++; }
	public void decRuleLevel() { ruleLevel--; }
	public tplexerParser(TokenStream input) {
		this(input, DebugEventSocketProxy.DEFAULT_DEBUGGER_PORT, new RecognizerSharedState());
	}
	public tplexerParser(TokenStream input, int port, RecognizerSharedState state) {
		super(input, state);
		DebugEventSocketProxy proxy =
			new DebugEventSocketProxy(this, port, null);

		setDebugListener(proxy);
		try {
			proxy.handshake();
		}
		catch (IOException ioe) {
			reportError(ioe);
		}
	}

	public tplexerParser(TokenStream input, DebugEventListener dbg) {
		super(input, dbg, new RecognizerSharedState());
	}

	protected boolean evalPredicate(boolean result, String predicate) {
		dbg.semanticPredicate(result, predicate);
		return result;
	}

	@Override public String[] getTokenNames() { return tplexerParser.tokenNames; }
	@Override public String getGrammarFileName() { return "/afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g"; }



	// $ANTLR start "start"
	// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:3:1: start : LETTER EOF ;
	public final void start() throws  {
		try { dbg.enterRule(getGrammarFileName(), "start");
		if ( getRuleLevel()==0 ) {dbg.commence();}
		incRuleLevel();
		dbg.location(3, 0);

		try {
			// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:3:7: ( LETTER EOF )
			dbg.enterAlt(1);

			// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:3:9: LETTER EOF
			{
			dbg.location(3,9);
			match(input,LETTER,FOLLOW_LETTER_in_start19); dbg.location(3,16);
			match(input,EOF,FOLLOW_EOF_in_start21); 
			}

		}
		catch (RecognitionException re) {
			reportError(re);
			recover(input,re);
		}
		finally {
			// do for sure before leaving
		}
		dbg.location(3, 18);

		}
		finally {
			dbg.exitRule(getGrammarFileName(), "start");
			decRuleLevel();
			if ( getRuleLevel()==0 ) {dbg.terminate();}
		}

	}
	// $ANTLR end "start"

	// Delegated rules



	public static final BitSet FOLLOW_LETTER_in_start19 = new BitSet(new long[]{0x0000000000000000L});
	public static final BitSet FOLLOW_EOF_in_start21 = new BitSet(new long[]{0x0000000000000002L});
}
