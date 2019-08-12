literal("A","<http://www.w3.org/2001/XMLSchema#string>").
iri("<alice>").
arc("<alice>","<name>","A").
shape(iri).
schema("<User>",iri).
schema("<Root>",iri).
label("<User>").
label("<Root>").
shapeMap("<alice>","<User>").
literal("B","<http://www.w3.org/2001/XMLSchema#string>").
iri("<bob>").
arc("<bob>","<name>","B").
shape(iri).
schema("<User>",iri).
schema("<Root>",iri).
label("<User>").
label("<Root>").
shapeMap("<bob>","<Root>").
hasShape(X, iri):-iri(X).
:- hasShape(X, iri), not iri(X).
      

#show result/2 .
result(X,Lbl) :- shapeMap(X,Lbl), schema(Lbl,S), hasShape(X,S).
result(X,no(Lbl)):- shapeMap(X,Lbl), schema(Lbl,S), not hasShape(X,S) .

% Remove CWA on hasShape
hasShape(X,S) | not hasShape(X,S):-node(X), shape(S) .

% #show countPropShape/4 .
countPropShape(X,P,S,T):-
 node(X), pred(P), shape(S),
 #count { V: arcWithShape(X,P,S,V) } = T .

% #show arcWithShape/4 .
arcWithShape(X,P,S,V):-arc(X,P,V),hasShape(V,S).

node(X):-arc(X,_,_).
node(X):-arc(_,_,X).
pred(P):-arc(_,P,_).
pred(P):-shape(qa(P,_,_,_)).

          
