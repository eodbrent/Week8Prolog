% adapted solution for eliminating duplicate results caused by having more than one parent or sibling
% found at - https://www.csee.umbc.edu/~finin/prolog/sibling/siblings.html
% Other resources used : 
%                       - https://stackoverflow.com/questions/64644967/recurse-for-kth-cousin-n-times-removed-in-prolog
%                       - https://stackoverflow.com/questions/39689283/calculating-cousin-relationship-in-prolog
:- use_module(library(clpfd)).
%****************************
%           FACTS         ***
%****************************
                        % *** relationships relative to me ***
male(herbert).          % GREAT GREAT GPA
male(ervin).            % Great GPA
male(dean).             % GPA
male(randy).            % Great uncle (in law)
male(george).           % Great uncle
male(brooke).           % First cousin once removed.
male(daniel).           % Second Cousin
male(mike).             % uncle
male(david).            % uncle
male(christopher).      % cousin
male(chris).            % cousin
male(jared).            % cousin
male(matt).             % cousin
male(justin).           % cousin
male(jeff).             % father
male(brent).            % me
male(reese).            % son
male(bear).             % son
male(brian).            % brother in-law
male(bodey).            % nephew
female(sue).            % GREAT GREAT GMA
female(elizabeth).      % Great GMA
female(mary).           % GMA
female(donna).          % Great Aunt
female(bon).            % Great Aunt
female(molly).          % aunt
female(gale).           % aunt
female(leanne).         % aunt
female(angela).         % cousin
female(jennifer).       % cousin
female(connie).         % mother
female(cassie).         % spouse
female(kierstyn).       % daughter
female(elli).           % daughter
female(lyla).           % grand-daughter
female(bonnie).         % sister
female(taylor).         % niece
% *** child is a child of parent   ***
% ***    child(child, parent)      ***
child(elizabeth, sue).
child(elizabeth, herbert).
child(mary, elizabeth).
child(mary, ervin).
child(george, elizabeth).
child(george, ervin).
child(donna, elizabeth).
child(donna, ervin).
child(bon, elizabeth).
child(molly, mary).
child(molly, dean).
child(mike, mary).
child(mike, dean).
child(david, mary).
child(david, dean).
child(connie, mary).
child(connie, dean).
child(brooke, donna).
child(brooke, randy).
child(daniel, brooke).
child(chris, gale).
child(chris, mike).
child(christopher, leanne).
child(christopher, david).
child(angela, gale).
child(angela, mike).
child(matt, molly).
child(jennifer, molly).
child(jared, molly).
child(justin, molly).
child(lyla, kierstyn).
child(kierstyn, cassie).
child(kierstyn, brent).
child(reese, cassie).
child(reese, brent).
child(elli, cassie).
child(elli, brent).
child(bear, cassie).
child(bear, brent).
child(brent, jeff).
child(brent, connie).
child(bonnie, jeff).
child(bonnie, connie).
child(bodey, bonnie).
child(bodey, brian).
child(taylor, bonnie).
child(taylor, brian).
%****************************
%           RULES         ***
%****************************
%***   IMMEDIATE FAMILY   ***
%****************************
parent(X, Y) :- child(Y, X).   % parent in case I need it to simplify
father(Y, X) :- child(Y, X), male(X).
mother(Y, X) :- child(Y, X), female(X).
together(X,Y) :- setof((X,Y), P^(child(P, X), child(P, Y), \+X=Y), To), member((X,Y), To), \+ (Y@<X, member((Y,X), To)). 
brother(X,Y) :- setof((X,Y), P^(child(X, P), child(Y, P), male(X), \+X=Y), Bro), member((X,Y), Bro), \+ (Y@<X, member((Y,X), Bro)).
sister(X,Y) :- setof((X,Y), P^(child(X, P), child(Y, P), female(X), \+X=Y), Sis), member((X,Y), Sis), \+ (Y@<X, member((Y,X), Sis)).
sibling(X,Y) :- setof((X,Y), P^(parent(P,X),parent(P,Y), \+X=Y), Sibs), member((X,Y), Sibs), \+ (Y@<X, member((Y,X), Sibs)).
inlaw(X, Y) :- sibling(Y, Z), together(Z, X). 
isSingleChild(X) :- child(X, _Y), \+ sibling(X, _Z), !.  
%****************************
% ***   EXTENDED FAMILY   ***    
%****************************                                            
grandma(Gma, Child_a) :- child(Child_a, Parent_a), child(Parent_a, Gma), female(Gma).
grandpa(Gpa, Child_a) :- child(Child_a, Parent_a), child(Parent_a, Gpa), male(Gpa).
grandparent(Parent_b, Child_a) :- child(Child_a, Parent_a), child(Parent_a, Parent_b).
aunt(Aunt_a, Child_a) :- (together(Aunt_a, Adult_a), child(Child_a, Adult_b), sibling(Adult_a, Adult_b), female(Aunt_a)) ; (child(Child_a, Adult_a), sibling(Aunt_a, Adult_a), female(Aunt_a)). 
uncle(Uncle_a, Child_a) :- (together(Uncle_a, Adult_a), child(Child_a, Adult_b), sibling(Adult_a, Adult_b), male(Uncle_a)) ; (child(Child_a, Adult_a), sibling(Uncle_a, Adult_a), male(Uncle_a)). 
nephew(Child_a, Adult_a) :- (together(Adult_a, Adult_b), sibling(Adult_b, Parent_a), child(Child_a, Parent_a)) ; (sibling(Adult_a, Parent_a), child(Child_a, Parent_a)), male(Child_a).
niece(Child_a, Adult_a) :- (together(Adult_a, Adult_b), sibling(Adult_b, Parent_a), child(Child_a, Parent_a)) ; (sibling(Adult_a, Parent_a), child(Child_a, Parent_a)), female(Child_a).
cousin(X,Y) :- setof((X,Y), P^(child(X, P), child(Y, Q), sibling(P, Q), \+X=Y), Cous), member((X,Y), Cous), \+ (Y@<X, member((Y,X), Cous)).
%****************************
% ***  GREATS AND BEYOND  ***
%****************************
great_grandparent(Parent_c, Child_a) :- child(Child_a, Parent_a), child(Parent_a, Parent_b), child(Parent_b, Parent_c).
great_aunt(G_Aunt_a, Child_a) :- grandparent(Parent_a, Child_a), sibling(G_Aunt_a, Parent_a), female(G_Aunt_a).
great_uncle(G_Uncle_a, Child_a) :- grandparent(Parent_a, Child_a), sibling(G_Uncle_a, Parent_a), male(G_Uncle_a).
great_grandpa(Great_gpa, Child_a) :- child(Child_a, Parent_a), child(Parent_a, Parent_b), child(Parent_b, Great_gpa), male(Great_gpa).
great_grandma(Great_gma, Child_a) :- child(Child_a, Parent_a), child(Parent_a, Parent_b), child(Parent_b, Great_gma), female(Great_gma).
secondcousin(Cousin_a, Cousin_b) :- great_grandparent(Parent_a, Cousin_a), great_grandparent(Parent_a, Cousin_b), \+ sibling(Cousin_a, Cousin_b), \+ cousin(Cousin_a, Cousin_b), Cousin_a \= Cousin_b.
cousin_once_removed(Cousin_a, Cousin_b) :- cousin(Cousin_c, Cousin_b), child(Cousin_a, Cousin_c).
second_cousin_twice_removed(Cousin_a, Cousin_b) :- child(Cousin_a, Parent_a), secondcousin(Parent_a, Cousin_b).
%****************************
% *** ANCESTOR/DESCENDANT ***
%****************************
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
descendant(X, Y) :- child(X, Y).
descendant(X, Y) :- child(X, Z), descendant(Z, Y).
%****************************
% ***       COUSINS       ***                               
%****************************
all_cousins(Person, N, ListOfCousins) :- setof(Cousin, nth_cousin_of(Person, N, Cousin), ListOfCousins).
% MEAT AND TATERS FOR all_cousins
nth_anchestor_of(Child,1,Xparent) :- child(Child, Xparent).
nth_anchestor_of(Child,N,Xparent) :- NN is N-1, NN>0, child(Child, InBetween), nth_anchestor_of(InBetween,NN,Xparent).
nth_cousin_of(Kid1,N, Kid2) :- nth_anchestor_of(Kid1,N,Xparent1), nth_anchestor_of(Kid2,N,Xparent2), sibling(Xparent1,Xparent2).  
removed(R, Direction) :- removed(R, Direction).
%****************************
% *** ALL COUSINS REMOVED ***
%****************************
all_cousinsRemoved(Person, N, removed(R, Direction), ListOfCousins) :-
        setof(Cousin, cousins_nth_removed(Person, Cousin, N, R), ListOfCousins).
% First Cousins -  (Cousin1, Cousin2, first, zero removed)
cousins_nth_removed(C1, C2, 1, 0) :- dif(C1, C2), dif(P1, P2), child(C1, P1), child(C2, P2), child(P1, GP), child(P2, GP).
% Any cousin, zero removed - (Cousin1, Cousin2, any, zero removed) N must be greater than 1, recursively checks until N is 1 (M #= N -1)
cousins_nth_removed(C1, C2, N, 0) :-
    N #> 1,
    dif(C1, C2),
    children_removed_ancestor(C1, C2, R, R),
    dif(P1, P2), 
    child(C1, P1),
    child(C2, P2),
    M #= N - 1,
    cousins_nth_removed(P1, P2, M, 0).
% Any cousin, any number removed - (Cousin1, Cousin2, any, any) R must be greater than 0, determines how far away each person is from their oldest ancestor to determine their number removed.
cousins_nth_removed(C1, C2, N, R) :-
    R #> 0,
    dif(C1, C2),
    children_removed_ancestor(C1, C2, R1, R2),
    R #= abs(R2 - R1), % R is equal too
    S #= R - 1, % S is equal too
    (   R1 #= R2, % R1 is equal too
        cousins_nth_removed(C1, C2, N, 0)
    ;   R1 #> R2, % R1 is greater than
        child(C1, P1),
        cousins_nth_removed(P1, C2, N, S)
    ;   R1 #< R2, % R1 is less than
        child(C2, P2),
        cousins_nth_removed(C1, P2, N, S)
    ).

children_removed_ancestor(C1, C2, R1, R2) :-
    child_removed_oldest_ancestor(C1, R1, A), 
    child_removed_oldest_ancestor(C2, R2, A).

child_removed_oldest_ancestor(C, 0, C) :- \+ child(C, _). % C is not a child of any parent...they are the oldest
child_removed_oldest_ancestor(C, N, A) :-
    N #> 0, % N is greater than 
    child(C, P),
    M #= N - 1, % M is equal too
    child_removed_oldest_ancestor(P, M, A).
/*
first attempts that yielded duplicate results (because I included both parents in most cases, instead of single parents):
sister(Child_a, Child_b) :- child(Child_a, Parent_a), child(Child_b, Parent_a), female(Child_a), Child_a \== Child_b.
brother(Child_a, Child_b) :- child(Child_a, Parent_a), child(Child_b, Parent_a), male(Child_a), Child_a \== Child_b.
together(Spouse_a, Spouse_b) :- child(Child_a, Spouse_a), child(Child_a, Spouse_b). 
cousin(Cousin_a, Cousin_b) :- child(Cousin_a, Parent_a), child(Cousin_b, Parent_b), sibling(Parent_a, Parent_b), Cousin_a \= Cousin_b.
sibling(Sib_a, Sib_b) :- child(Sib_a, Parent_a), child(Sib_b, Parent_a), Sib_a \= Sib_b.
*/
