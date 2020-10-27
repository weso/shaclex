hasShape(X,and(S1,S2)):-
 shape(and(S1,S2)), hasShape(X,S1), hasShape(X,S2) .
:-hasShape(X,and(S1,_)), not hasShape(X,S1) .
:-hasShape(X,and(_,S2)), not hasShape(X,S2) .
      
hasShape(X, iri):-iri(X).
:- hasShape(X, iri), not iri(X).
      
hasShape(X,qa(P,S,Min,star)):-
 shape(qa(P,S,Min,star)),
 countPropShape(X,P,S,C), Min <= C .
:- hasShape(X,qa(P,S,Min,star)), countPropShape(X,P,S,C), C < Min .
      
hasShape(X,qa(P,S,Min,intMax(Max))):-
 shape(qa(P,S,Min,intMax(Max))),
 countPropShape(X,P,S,C), Min <= C, C <= Max .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C < Min .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C > Max .
      
hasShape(X,no(S)):- node(X), shape(no(S)), not hasShape(X,S) .
:- hasShape(X, no(S)), hasShape(X,S) .
      

#show result/2 .
result(X,Lbl) :- shapeMap(X,Lbl), schema(Lbl,S), hasShape(X,S).
result(X,no(Lbl)):- shapeMap(X,Lbl), schema(Lbl,S), not hasShape(X,S) .

% Remove CWA on hasShape
hasShape(X,S) | not hasShape(X,S):-node(X), shape(S) .

% #show countPropShape/4 .
countPropShape(X,P,S,T):-
 node(X), pred(P), shape(S),
 #count { V: arcWithShape(X,P,S,V) } = T .

countPropShape(X,P,S,0):-
 node(X),
 pred(P),
 shape(S),
 not arcWithShape(X,P,S,_).

% #show arcWithShape/4 .
arcWithShape(X,P,S,V):-arc(X,P,V),hasShape(V,S).

node(X):-shapeMap(X,_).
node(X):-arc(X,_,_).
node(X):-arc(_,_,X).
pred(P):-arc(_,P,_).
pred(P):-shape(qa(P,_,_,_)).

          
