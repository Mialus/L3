// $ANTLR 3.5 /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g 2015-01-20 14:13:12

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;

@SuppressWarnings("all")
public class tplexerLexer extends Lexer {
	public static final int EOF=-1;
	public static final int LETTER=4;
	public static final int WS=5;

	// delegates
	// delegators
	public Lexer[] getDelegates() {
		return new Lexer[] {};
	}

	public tplexerLexer() {} 
	public tplexerLexer(CharStream input) {
		this(input, new RecognizerSharedState());
	}
	public tplexerLexer(CharStream input, RecognizerSharedState state) {
		super(input,state);
	}
	@Override public String getGrammarFileName() { return "/afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g"; }

	// $ANTLR start "LETTER"
	public final void mLETTER() throws RecognitionException {
		try {
			int _type = LETTER;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:2:9: ( 'a' .. 'z' )
			// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:
			{
			if ( (input.LA(1) >= 'a' && input.LA(1) <= 'z') ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "LETTER"

	// $ANTLR start "WS"
	public final void mWS() throws RecognitionException {
		try {
			int _type = WS;
			int _channel = DEFAULT_TOKEN_CHANNEL;
			// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:4:3: ( ( ' ' | '\\t' | '\\r' | '\\n' ) )
			// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:4:4: ( ' ' | '\\t' | '\\r' | '\\n' )
			{
			if ( (input.LA(1) >= '\t' && input.LA(1) <= '\n')||input.LA(1)=='\r'||input.LA(1)==' ' ) {
				input.consume();
			}
			else {
				MismatchedSetException mse = new MismatchedSetException(null,input);
				recover(mse);
				throw mse;
			}
			_channel=HIDDEN;
			}

			state.type = _type;
			state.channel = _channel;
		}
		finally {
			// do for sure before leaving
		}
	}
	// $ANTLR end "WS"

	@Override
	public void mTokens() throws RecognitionException {
		// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:1:8: ( LETTER | WS )
		int alt1=2;
		int LA1_0 = input.LA(1);
		if ( ((LA1_0 >= 'a' && LA1_0 <= 'z')) ) {
			alt1=1;
		}
		else if ( ((LA1_0 >= '\t' && LA1_0 <= '\n')||LA1_0=='\r'||LA1_0==' ') ) {
			alt1=2;
		}

		else {
			NoViableAltException nvae =
				new NoViableAltException("", 1, 0, input);
			throw nvae;
		}

		switch (alt1) {
			case 1 :
				// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:1:10: LETTER
				{
				mLETTER(); 

				}
				break;
			case 2 :
				// /afs/deptinfo-st.univ-fcomte.fr/users/pwargnie/L3/AS/TP-as/TP1/tplexer.g:1:17: WS
				{
				mWS(); 

				}
				break;

		}
	}



}
