readFile(List):- open('numbers.txt',read,Str),
         read(Str,List).

list_to_PQ(L,PQ) :-
   list_to_PQ(L,[],PQ).

list_to_PQ([],P,P).
list_to_PQ([H|T],P1,P3) :-
   insert(P1,H,P2),
   list_to_PQ(T,P2,P3).

insert([],X,[X]).
insert(L,X,L2):- append(L,[X],L1),length(L1,S),heapify(S,L1,L2).

%nth1(S,L1,Z),write(Z),nl.


list_i_j_swapped(As,I,J,Cs) :-
   same_length(As,Cs),
   append(BeforeI,[AtI|PastI],As),
   append(BeforeI,[AtJ|PastI],Bs),
   append(BeforeJ,[AtJ|PastJ],Bs),
   append(BeforeJ,[AtI|PastJ],Cs),
   length(BeforeI,I),
   length(BeforeJ,J).

%heapify(0,L,L).
%Indexing is 1 based.
heapify(1,L,L).
heapify(S,L,L) :-
    S \= 1,
    Pr is S//2,
    nth1(Pr,L,PVal),
    nth1(S,L,CVal),
    CVal @>= PVal.

heapify(S,L,L1) :-
    S \= 1,
    Pr is S//2,
    nth1(Pr,L,PVal),
    nth1(S,L,CVal),
    CVal @< PVal,
    NewPr is Pr-1,
    NewCr is S-1,
    list_i_j_swapped(L,NewPr,NewCr,L2),
    heapify(Pr,L2,L1).


extract_min([L],L,[]).
extract_min(L,V,L1):-
    length(L,S),
    NewL is S-1,
    nth1(1,L,V),
    list_i_j_swapped(L,NewL,0,L2),
    remove_last(L2,L3),
    heapify2(1,L3,L1).

remove_last(M,Y) :- reverse(M,T1),get_tail(T1,T),reverse(T,Y).
get_tail([_|T],T).

% I is the index , L is the list and NL is the new list
% S is the size
heapify2(I,L,L) :- length(L,S), H is S//2 , I @> H ,I @=< S.

heapify2(I,L,NL) :-
                    length(L,S),I =< S//2,
                    Lft is 2*I,
                    nth1(Lft,L,LVal),
                    Rgt is 2*I+1,
                    nth1(Rgt,L,RVal), LVal<RVal,
                    nth1(I,L,CVal), LVal<CVal , NLft is Lft-1,
                    NCr is I-1,
                    list_i_j_swapped(L,NCr,NLft,L1),
                    heapify2(Lft,L1,NL).
heapify2(I,L,NL) :-length(L,S),I =< S//2,
                    Lft is 2*I,
                    nth1(Lft,L,LVal),
                    Rgt is 2*I+1,
                    nth1(Rgt,L,RVal), RVal<LVal,
                    nth1(I,L,CVal), RVal<CVal , NRgt is Rgt-1,
                    NCr is I-1,
                    list_i_j_swapped(L,NCr,NRgt,L1),
                    heapify2(Rgt,L1,NL).
heapify2(I,L,NL) :-length(L,S),I =< S//2,
                    Rgt is 2*I+1,
                    nth1(Rgt,L,RVal),
                    nth1(I,L,CVal), RVal<CVal , NRgt is Rgt-1,
                    NCr is I-1,
                    list_i_j_swapped(L,NCr,NRgt,L1),
                    heapify2(Rgt,L1,NL).
heapify2(I,L,NL) :-
                    length(L,S),I =< S//2,
                    Lft is 2*I,
                    nth1(Lft,L,LVal),
                    nth1(I,L,CVal), LVal<CVal , NLft is Lft-1,
                    NCr is I-1,
                    list_i_j_swapped(L,NCr,NLft,L1),
                    heapify2(Lft,L1,NL).
