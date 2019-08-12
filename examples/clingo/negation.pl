literal("A","<http://www.w3.org/2001/XMLSchema#string>").
iri("<alice>").
arc("<alice>","<name>","A").
arc("<alice>","<knows>","<alice>").
shape(no(datatype("<http://www.w3.org/2001/XMLSchema#string>"))).
shape(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star)).
shape(no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))).
shape(datatype("<http://www.w3.org/2001/XMLSchema#string>")).
shape(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1))).
shape(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star)))).
shape(and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true)).
shape(true).
shape(no(ref("<User>"))).
shape(qa("<knows>",no(ref("<User>")),1,star)).
shape(no(qa("<knows>",no(ref("<User>")),1,star))).
shape(ref("<User>")).
shape(qa("<knows>",ref("<User>"),0,star)).
shape(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star)))).
shape(and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)).
shape(iri).
shape(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true))).
shape(and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<User>",and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<Root>",no(and(qa("<knows>",ref("<User>"),1,star),no(qa("<knows>",no(ref("<User>")),1,star))))).
label("<User>").
label("<Root>").
shapeMap("<alice>","<User>").
hasShape(X,qa(P,S,Min,star)):-
 shape(qa(P,S,Min,star)),
 countPropShape(X,P,S,C), Min <= C .
:- hasShape(X,qa(P,S,Min,star)), countPropShape(X,P,S,C), C < Min .
      
hasShape(X,qa(P,S,Min,intMax(Max))):-
 shape(qa(P,S,Min,intMax(Max))),
 countPropShape(X,P,S,C), Min <= C, C <= Max .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C < Min .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C > Max .
      
hasShape(X, datatype(D)):- shape(datatype(D)), literal(X,D).
:- hasShape(X, datatype(D)), not literal(X,D).
      
hasShape(X, iri):-iri(X).
:- hasShape(X, iri), not iri(X).
      
hasShape(X,ref(Lbl)):-
  shape(ref(Lbl)), schema(Lbl,S), hasShape(X,S).
:-hasShape(X,ref(Lbl)), schema(Lbl,S), not hasShape(X,S) .
      
hasShape(X,no(S)):- node(X), shape(no(S)), not hasShape(X,S) .
:- hasShape(X, no(S)), hasShape(X,S) .
      
hasShape(X,true):-node(X) .
      
hasShape(X,and(S1,S2)):-
 shape(and(S1,S2)), hasShape(X,S1), hasShape(X,S2) .
:-hasShape(X,and(S1,_)), not hasShape(X,S1) .
:-hasShape(X,and(_,S2)), not hasShape(X,S2) .
      

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

          
literal("A","<http://www.w3.org/2001/XMLSchema#string>").
literal("B","<http://www.w3.org/2001/XMLSchema#string>").
iri("<bob>").
arc("<bob>","<name>","B").
arc("<bob>","<name>","A").
shape(no(datatype("<http://www.w3.org/2001/XMLSchema#string>"))).
shape(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star)).
shape(no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))).
shape(datatype("<http://www.w3.org/2001/XMLSchema#string>")).
shape(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1))).
shape(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star)))).
shape(and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true)).
shape(true).
shape(no(ref("<User>"))).
shape(qa("<knows>",no(ref("<User>")),1,star)).
shape(no(qa("<knows>",no(ref("<User>")),1,star))).
shape(ref("<User>")).
shape(qa("<knows>",ref("<User>"),0,star)).
shape(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star)))).
shape(and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)).
shape(iri).
shape(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true))).
shape(and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<User>",and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<Root>",no(and(qa("<knows>",ref("<User>"),1,star),no(qa("<knows>",no(ref("<User>")),1,star))))).
label("<User>").
label("<Root>").
shapeMap("<bob>","<User>").
hasShape(X,qa(P,S,Min,star)):-
 shape(qa(P,S,Min,star)),
 countPropShape(X,P,S,C), Min <= C .
:- hasShape(X,qa(P,S,Min,star)), countPropShape(X,P,S,C), C < Min .
      
hasShape(X,qa(P,S,Min,intMax(Max))):-
 shape(qa(P,S,Min,intMax(Max))),
 countPropShape(X,P,S,C), Min <= C, C <= Max .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C < Min .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C > Max .
      
hasShape(X, datatype(D)):- shape(datatype(D)), literal(X,D).
:- hasShape(X, datatype(D)), not literal(X,D).
      
hasShape(X, iri):-iri(X).
:- hasShape(X, iri), not iri(X).
      
hasShape(X,ref(Lbl)):-
  shape(ref(Lbl)), schema(Lbl,S), hasShape(X,S).
:-hasShape(X,ref(Lbl)), schema(Lbl,S), not hasShape(X,S) .
      
hasShape(X,no(S)):- node(X), shape(no(S)), not hasShape(X,S) .
:- hasShape(X, no(S)), hasShape(X,S) .
      
hasShape(X,true):-node(X) .
      
hasShape(X,and(S1,S2)):-
 shape(and(S1,S2)), hasShape(X,S1), hasShape(X,S2) .
:-hasShape(X,and(S1,_)), not hasShape(X,S1) .
:-hasShape(X,and(_,S2)), not hasShape(X,S2) .
      

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

          
literal("C","<http://www.w3.org/2001/XMLSchema#string>").
iri("<carol>").
arc("<carol>","<name>","C").
shape(no(datatype("<http://www.w3.org/2001/XMLSchema#string>"))).
shape(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star)).
shape(no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))).
shape(datatype("<http://www.w3.org/2001/XMLSchema#string>")).
shape(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1))).
shape(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star)))).
shape(and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true)).
shape(true).
shape(no(ref("<User>"))).
shape(qa("<knows>",no(ref("<User>")),1,star)).
shape(no(qa("<knows>",no(ref("<User>")),1,star))).
shape(ref("<User>")).
shape(qa("<knows>",ref("<User>"),0,star)).
shape(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star)))).
shape(and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)).
shape(iri).
shape(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true))).
shape(and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<User>",and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<Root>",no(and(qa("<knows>",ref("<User>"),1,star),no(qa("<knows>",no(ref("<User>")),1,star))))).
label("<User>").
label("<Root>").
shapeMap("<carol>","<User>").
hasShape(X,qa(P,S,Min,star)):-
 shape(qa(P,S,Min,star)),
 countPropShape(X,P,S,C), Min <= C .
:- hasShape(X,qa(P,S,Min,star)), countPropShape(X,P,S,C), C < Min .
      
hasShape(X,qa(P,S,Min,intMax(Max))):-
 shape(qa(P,S,Min,intMax(Max))),
 countPropShape(X,P,S,C), Min <= C, C <= Max .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C < Min .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C > Max .
      
hasShape(X, datatype(D)):- shape(datatype(D)), literal(X,D).
:- hasShape(X, datatype(D)), not literal(X,D).
      
hasShape(X, iri):-iri(X).
:- hasShape(X, iri), not iri(X).
      
hasShape(X,ref(Lbl)):-
  shape(ref(Lbl)), schema(Lbl,S), hasShape(X,S).
:-hasShape(X,ref(Lbl)), schema(Lbl,S), not hasShape(X,S) .
      
hasShape(X,no(S)):- node(X), shape(no(S)), not hasShape(X,S) .
:- hasShape(X, no(S)), hasShape(X,S) .
      
hasShape(X,true):-node(X) .
      
hasShape(X,and(S1,S2)):-
 shape(and(S1,S2)), hasShape(X,S1), hasShape(X,S2) .
:-hasShape(X,and(S1,_)), not hasShape(X,S1) .
:-hasShape(X,and(_,S2)), not hasShape(X,S2) .
      

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

          
literal("A","<http://www.w3.org/2001/XMLSchema#string>").
iri("<alice>").
arc("<alice>","<name>","A").
arc("<alice>","<knows>","<alice>").
shape(no(ref("<User>"))).
shape(qa("<knows>",no(ref("<User>")),1,star)).
shape(no(qa("<knows>",no(ref("<User>")),1,star))).
shape(ref("<User>")).
shape(qa("<knows>",ref("<User>"),1,star)).
shape(and(qa("<knows>",ref("<User>"),1,star),no(qa("<knows>",no(ref("<User>")),1,star)))).
shape(no(and(qa("<knows>",ref("<User>"),1,star),no(qa("<knows>",no(ref("<User>")),1,star))))).
schema("<User>",and(and(iri,and(and(qa("<knows>",ref("<User>"),0,star),no(qa("<knows>",no(ref("<User>")),1,star))),true)),and(and(qa("<name>",datatype("<http://www.w3.org/2001/XMLSchema#string>"),1,intMax(1)),no(qa("<name>",no(datatype("<http://www.w3.org/2001/XMLSchema#string>")),1,star))),true))).
schema("<Root>",no(and(qa("<knows>",ref("<User>"),1,star),no(qa("<knows>",no(ref("<User>")),1,star))))).
label("<User>").
label("<Root>").
shapeMap("<alice>","<Root>").
hasShape(X,qa(P,S,Min,star)):-
 shape(qa(P,S,Min,star)),
 countPropShape(X,P,S,C), Min <= C .
:- hasShape(X,qa(P,S,Min,star)), countPropShape(X,P,S,C), C < Min .
      
hasShape(X,qa(P,S,Min,intMax(Max))):-
 shape(qa(P,S,Min,intMax(Max))),
 countPropShape(X,P,S,C), Min <= C, C <= Max .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C < Min .
:- hasShape(X,qa(P,S,Min,intMax(Max))), countPropShape(X,P,S,C), C > Max .
      
hasShape(X, datatype(D)):- shape(datatype(D)), literal(X,D).
:- hasShape(X, datatype(D)), not literal(X,D).
      
hasShape(X, iri):-iri(X).
:- hasShape(X, iri), not iri(X).
      
hasShape(X,ref(Lbl)):-
  shape(ref(Lbl)), schema(Lbl,S), hasShape(X,S).
:-hasShape(X,ref(Lbl)), schema(Lbl,S), not hasShape(X,S) .
      
hasShape(X,no(S)):- node(X), shape(no(S)), not hasShape(X,S) .
:- hasShape(X, no(S)), hasShape(X,S) .
      
hasShape(X,true):-node(X) .
      
hasShape(X,and(S1,S2)):-
 shape(and(S1,S2)), hasShape(X,S1), hasShape(X,S2) .
:-hasShape(X,and(S1,_)), not hasShape(X,S1) .
:-hasShape(X,and(_,S2)), not hasShape(X,S2) .
      

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

          
