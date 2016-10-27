// ANTLR4 Equivalent of accompanying bnf, developed in
// http://www.w3.org/2005/01/yacker/uploads/ShEx3
// Updated to Jul 27 AM ShEx3
// Updated to Aug 23 AM ShEx3 (last change was EGP 20150820)
// Sept 21 AM disallow single internal unary (e.g. {(:p .{2}){3}}
//            Change (non-standard) "codeLabel" to "productionName"
// Oct 26 - change annotation predicate to include rdftype (how did this slip in to the production rules?
// Dec 30 - update to match http://www.w3.org/2005/01/yacker/uploads/ShEx2/bnf with last change "EGP 20151120"
// May 23, 2016 - Update to match http://www.w3.org/2005/01/yacker/uploads/ShEx2/bnf with last change "EGP20160520" AND ';' separator and '//' for annotations
// May 24, 2016 - EGP20150424
// Aug 11, 2016 - EGP20160708
// Sep 14, 2016 - Revised to match Eric's latest reshuffle
// Sep 24, 2016 - Switched to TT grammar (vs inner and outer shapes)
// Sep 26, 2016 - Refactored to match https://raw.githubusercontent.com/shexSpec/shex.js/7eb770fe2b5bab9edfe9558dc07bb6f6dcdf5d23/doc/bnf
// Oct 27, 2016 - Added comments to '*', '*' and '?' to facilitate parsing
// Oct 27, 2016 - Added qualifier rule tobe reused by shapeDefinition and inlineShapeDefinition
// Oct 27, 2016 - Added negation rule

grammar ShExDoc;

shExDoc 		: directive* ((notStartAction | startActions) statement*)? EOF;  // leading CODE
directive       : baseDecl
				| prefixDecl
				;
baseDecl 		: KW_BASE  IRIREF ;
prefixDecl		: KW_PREFIX PNAME_NS IRIREF ;
notStartAction  : start | shapeExprDecl ;
start           : KW_START '=' shapeExpression ;
startActions	: codeDecl+ ;
statement 		: directive | notStartAction ;
shapeExprDecl   : shapeLabel (shapeExpression | KW_EXTERNAL) ;
shapeExpression : shapeOr ;
shapeOr  		: shapeAnd (KW_OR shapeAnd)* ;
shapeAnd		: shapeNot (KW_AND shapeNot)* ;
shapeNot	    : negation? shapeAtom ;
negation        : KW_NOT | '!' ;
inlineShapeExpression : inlineShapeOr ;
inlineShapeOr   : inlineShapeAnd (KW_OR inlineShapeAnd)* ;
inlineShapeAnd  : inlineShapeNot (KW_AND inlineShapeNot)* ;
inlineShapeNot  : negation? inlineShapeAtom ;
inlineShapeDefinition : qualifier* '{' someOfShape? '}' ;
shapeDefinition : qualifier* '{' someOfShape? '}' annotation* semanticActions ;
qualifier       : includeSet | extraPropertySet | KW_CLOSED ;
extraPropertySet : KW_EXTRA predicate+ ;
someOfShape     : groupShape
				| multiElementSomeOf
				;
multiElementSomeOf : groupShape ( '|' groupShape)+ ;
innerShape      : multiElementGroup
				| multiElementSomeOf
				;
groupShape      : singleElementGroup
				| multiElementGroup
				;
singleElementGroup : unaryShape ';'? ;
multiElementGroup : unaryShape (';' unaryShape)+ ';'? ;
unaryShape      : productionLabel? (tripleConstraint | encapsulatedShape)
				| include
				;
encapsulatedShape  : '(' innerShape ')' cardinality? annotation* semanticActions ;
shapeAtom		: nodeConstraint shapeOrRef?    # shapeAtomNodeConstraint
				| shapeOrRef                    # shapeAtomShapeOrRef
				| '(' shapeExpression ')'		# shapeAtomShapeExpression
				| '.'							# shapeAtomAny			// no constraint
				;
inlineShapeAtom : nodeConstraint inlineShapeOrRef? # inlineShapeAtomNodeConstraint
				| inlineShapeOrRef nodeConstraint? # inlineShapeAtomShapeOrRef
				| '(' shapeExpression ')'		# inlineShapeAtomShapeExpression
				| '.'							# inlineShapeAtomAny   // no constraint
				;
nodeConstraint  : KW_LITERAL xsFacet*			# nodeConstraintLiteral
				| nonLiteralKind stringFacet*	# nodeConstraintNonLiteral
				| datatype xsFacet*				# nodeConstraintDatatype
				| valueSet xsFacet*				# nodeConstraintValueSet
				| xsFacet+						# nodeConstraintFacet
				;
nonLiteralKind  : KW_IRI
				| KW_BNODE
				| KW_NONLITERAL
				;
xsFacet			: stringFacet
				| numericFacet;
stringFacet     : stringLength INTEGER
			    | KW_PATTERN string
				| '~' string			// shortcut for "PATTERN"
				;
stringLength	: KW_LENGTH
				| KW_MINLENGTH
				| KW_MAXLENGTH;
numericFacet	: numericRange (numericLiteral | string '^^' datatype)
				| numericLength INTEGER
				;
numericRange	: KW_MININCLUSIVE
			    | KW_MINEXCLUSIVE
			    | KW_MAXINCLUSIVE
			    | KW_MAXEXCLUSIVE
			    ;
numericLength   : KW_TOTALDIGITS
				| KW_FRACTIONDIGITS
				;
tripleConstraint : senseFlags? predicate inlineShapeExpression cardinality? annotation* semanticActions ;
senseFlags      : '!' '^'?
				| '^' '!'?		// inverse not
				;
valueSet		: '[' valueSetValue* ']' ;
valueSetValue   : iriRange
				| literal
				;
iriRange        : iri ('~' exclusion*)?
				| '.' exclusion+
				;
exclusion       : '-' iri '~'? ;
literal         : rdfLiteral
				| numericLiteral
				| booleanLiteral
				;
shapeOrRef      : ATPNAME_LN
				| ATPNAME_NS
				| '@' shapeLabel
				| shapeDefinition
				;
inlineShapeOrRef : ATPNAME_LN
				| ATPNAME_NS
				| '@' shapeLabel
				| inlineShapeDefinition
				;
include			: '&' shapeLabel ;
semanticActions	: codeDecl* ;
annotation      : '//' predicate (iri | literal) ;
// BNF: predicate ::= iri | RDF_TYPE
predicate       : iri
				| rdfType
				;
rdfType			: RDF_TYPE ;
datatype        : iri ;
cardinality     :  '*'         # starCardinality
				| '+'          # plusCardinality
				| '?'          # optionalCardinality
				| repeatRange  # repeatCardinality
				;
// BNF: REPEAT_RANGE ::= '{' INTEGER (',' (INTEGER | '*')?)? '}'
repeatRange     : '{' min_range (',' max_range?)? '}' ;
min_range       : INTEGER ;
max_range       : INTEGER
				| '*'
				;
shapeLabel      : iri
				| blankNode
				;
numericLiteral  : INTEGER
				| DECIMAL
				| DOUBLE
				;
rdfLiteral      : string (LANGTAG | '^^' datatype)? ;
booleanLiteral  : KW_TRUE
				| KW_FALSE
				;
string          : STRING_LITERAL_LONG1
                | STRING_LITERAL_LONG2
                | STRING_LITERAL1
				| STRING_LITERAL2
				;
iri             : IRIREF
				| prefixedName
				;
prefixedName    : PNAME_LN
				| PNAME_NS
				;
blankNode       : BLANK_NODE_LABEL ;
codeDecl		: '%' iri (CODE | '%') ;
productionLabel : '$' (iri | blankNode) ;

// Reserved for future use
includeSet      : '&' shapeLabel+ ;


// Keywords
KW_BASE 			: B A S E ;
KW_EXTERNAL			: E X T E R N A L ;
KW_PREFIX       	: P R E F I X ;
KW_START        	: S T A R T ;
KW_VIRTUAL      	: V I R T U A L ;
KW_CLOSED       	: C L O S E D ;
KW_EXTRA        	: E X T R A ;
KW_LITERAL      	: L I T E R A L ;
KW_IRI          	: I R I ;
KW_NONLITERAL   	: N O N L I T E R A L ;
KW_PATTERN      	: P A T T E R N ;
KW_BNODE        	: B N O D E ;
KW_AND          	: A N D ;
KW_OR           	: O R ;
KW_MININCLUSIVE 	: M I N I N C L U S I V E ;
KW_MINEXCLUSIVE 	: M I N E X C L U S I V E ;
KW_MAXINCLUSIVE 	: M A X I N C L U S I V E ;
KW_MAXEXCLUSIVE 	: M A X E X C L U S I V E ;
KW_LENGTH       	: L E N G T H ;
KW_MINLENGTH    	: M I N L E N G T H ;
KW_MAXLENGTH    	: M A X L E N G T H ;
KW_TOTALDIGITS  	: T O T A L D I G I T S ;
KW_FRACTIONDIGITS 	: F R A C T I O N D I G I T S ;
KW_NOT				: N O T ;
KW_TRUE         	: 'true' ;
KW_FALSE        	: 'false' ;

// terminals
PASS				  : [ \t\r\n]+ -> skip;
COMMENT				  : '#' ~[\r\n]* -> skip;

CODE                  : '{' (~[%\\] | '\\' [%\\] | UCHAR)* '%' '}' ;
RDF_TYPE              : 'a' ;
IRIREF                : '<' (~[\u0000-\u0020=<>\"{}|^`\\] | UCHAR)* '>' ; /* #x00=NULL #01-#x1F=control codes #x20=space */
PNAME_NS              : PN_PREFIX? ':' ;
PNAME_LN              : PNAME_NS PN_LOCAL ;
ATPNAME_NS			  : '@' PN_PREFIX? ':' ;
ATPNAME_LN			  : '@' PNAME_NS PN_LOCAL ;
BLANK_NODE_LABEL      : '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)? ;
LANGTAG               : '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)* ;
INTEGER               : [+-]? [0-9]+ ;
DECIMAL               : [+-]? [0-9]* '.' [0-9]+ ;
DOUBLE                : [+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.'? [0-9]+ EXPONENT) ;

fragment EXPONENT     : [eE] [+-]? [0-9]+ ;

STRING_LITERAL1       : '\'' (~[\u0027\u005C\u000A\u000D] | ECHAR | UCHAR)* '\'' ; /* #x27=' #x5C=\ #xA=new line #xD=carriage return */
STRING_LITERAL2       : '"' (~[\u0022\u005C\u000A\u000D] | ECHAR | UCHAR)* '"' ;   /* #x22=" #x5C=\ #xA=new line #xD=carriage return */
STRING_LITERAL_LONG1  : '\'\'\'' (('\'' | '\'\'')? (~[\'\\] | ECHAR | UCHAR))* '\'\'\'' ;
STRING_LITERAL_LONG2  : '"""' (('"' | '""')? (~[\"\\] | ECHAR | UCHAR))* '"""' ;

fragment UCHAR                 : '\\u' HEX HEX HEX HEX | '\\U' HEX HEX HEX HEX HEX HEX HEX HEX ;
fragment ECHAR                 : '\\' [tbnrf\\\"\'] ;
fragment WS                    : [\u0020\u0009\u000D\u000A] ; /* #x20=space #x9=character tabulation #xD=carriage return #xA=new line */

fragment PN_CHARS_BASE 		   : [A-Z] | [a-z] | [\u00C0-\u00D6] | [\u00D8-\u00F6] | [\u00F8-\u02FF] | [\u0370-\u037D]
					   		   | [\u037F-\u1FFF] | [\u200C-\u200D] | [\u2070-\u218F] | [\u2C00-\u2FEF] | [\u3001-\uD7FF]
					           | [\uF900-\uFDCF] | [\uFDF0-\uFFFD]
					   		   ;
fragment PN_CHARS_U            : PN_CHARS_BASE | '_' ;
fragment PN_CHARS              : PN_CHARS_U | '-' | [0-9] | [\u00B7] | [\u0300-\u036F] | [\u203F-\u2040] ;
fragment PN_PREFIX             : PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)? ;
fragment PN_LOCAL              : (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))? ;
fragment PLX                   : PERCENT | PN_LOCAL_ESC ;
fragment PERCENT               : '%' HEX HEX ;
fragment HEX                   : [0-9] | [A-F] | [a-f] ;
fragment PN_LOCAL_ESC          : '\\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ','
					  		   | ';' | '=' | '/' | '?' | '#' | '@' | '%') ;

fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
