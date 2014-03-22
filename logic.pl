% Define a procedure to see if in a list there are repetitions of a same number
% -----------------------------------------------------------------------------
checkDuplicates([]).
checkDuplicates([A|L]):- hasElement(A,L), checkDuplicates(L).

hasElement(_,[]).
hasElement(A,[B|L]):- A\==B, hasElement(A,L).


% Define procedure to find permutations
% -----------------------------------------------------------------------------
permutation([],[]).
permutation(L1,[A|L2]):- permutation_accessory(A,L1,L3), permutation(L3,L2).

permutation_accessory(X,[X|L],L).
permutation_accessory(X,[E|L1],[E|L2]):- permutation_accessory(X,L1,L2).


% Define the standard full list
% -----------------------------------------------------------------------------
full(A):- permutation(A,[1,2,3,4,5,6,7,8,9]),!.
%full(A):- permutation(A,[1,2,3,4]),!.


% Get a full list without elements who can't be used.
% -----------------------------------------------------------------------------
getRemaining(L,R):-full(F), getRemaining(L,R,F).

getRemaining([],L,L).
getRemaining([A|L],R,F):-deleteFromList(A,F,NF), getRemaining(L,R,NF).

deleteFromList(_,[],[]).
deleteFromList(X,[X|L1],NL) :- deleteFromList(X,L1,NL).
deleteFromList(X,[E|L1],[E|NL]) :- not(X = E), deleteFromList(X,L1,NL).


% Define the associated operation
% -----------------------------------------------------------------------------
checkIfSatisfied(N,F,V):-T=..[F,N,V], T.


% Define the possible mathematical operations:
% -----------------------------------------------------------------------------
%OP WHERE DISTRIBUTIVITY HOLDS
sum(X,[X]).
sum(X,[A,B|L]):-C is A+B, sum(X,[C|L]).

multiplication(X,[X]).
multiplication(X,[A,B|L]):-C is A*B, multiplication(X,[C|L]).

%OP WHERE DISTRIBUTIVITY DOES NOT HOLD
subtraction(A,L):-permutation(L,L1), subtraction_perform(A,L1), !.

subtraction_perform(X,[X]).
subtraction_perform(X,[A,B|L]):-C is A-B, subtraction_perform(X,[C|L]).

division(A,L):-permutation(L,L1), division_perform(A,L1), !.

division_perform(X,[X]).
division_perform(X,[A,B|L]):-C is A/B, division_perform(X,[C|L]).


% Define alternative naming for operations:
% -----------------------------------------------------------------------------
*(A,B) :- multiplication(A,B).
/(A,B) :- division(A,B).
+(A,B) :- sum(A,B).
-(A,B) :- subtraction(A,B).


% Get a list from a bigger list based on the number of elements you want to have. (this can be done way more efficently!!!!).
% -----------------------------------------------------------------------------

mightyList(L,N,NL):-permutation(L,TL), mightyListReducer(TL,N,NL).

mightyListReducer([E|_],N,[E]):- N =:= 1,!.
mightyListReducer([E|L],N,[E|L1]):- mightyListReducer(L,N-1,L1).

differentMightyList(L,N,[NL|RL]):- mightyList(L,N,NL),!, differentMightyList(L,N,[NL],RL).

differentMightyList(OL,N,L,[NL|L2]):- mightyList(OL,N,NL), not(equalList(NL,L)), !, differentMightyList(OL,N,[NL|L],L2).
differentMightyList(_,_,_,[]).

equalList(L,[P|_]):-permutation(L,P),!.
equalList(L,[_|L2]):-equalList(L,L2).


% Get list of possible combinations to satisfy a given condition.
% -----------------------------------------------------------------------------
%possibilities(Result,NOfOperators,AlreadyUsedNumbers,Function,X):- getRemaining(AlreadyUsedNumbers, NL), differentMightyList(NL,NOfOperators,ListOfPossibilities), possibilities_accessory(Result,Function,ListOfPossibilities,X).

possibilities(R,N,_,_,[R]):- N is 1, !.
possibilities(R,N,L,F,X):- getRemaining(L, NL), differentMightyList(NL,N,P), possibilities_accessory(R,F,P,X).

possibilities_accessory(R,F,[L|_],L):- checkIfSatisfied(R,F,L).
possibilities_accessory(R,F,[_|L],X):- possibilities_accessory(R,F,L,X).


% Combine two lsts.
% -----------------------------------------------------------------------------

combineList([],L,L).
combineList([A|L1],L2,[A|L]):-combineList(L1,L2,L).


% Play by condition
% -----------------------------------------------------------------------------

conditions1(R1,M1,C1,Result):- possibilities(R1,M1,[],C1,Result).
conditions2(R1,M1,C1,R2,M2,C2,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,Result).
conditions3(R1,M1,C1,R2,M2,C2,R3,M3,C3,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,T3),
                    possibilities(R3,M3,T3,C3,T4), combineList(T3,T4,Result).
conditions4(R1,M1,C1,R2,M2,C2,R3,M3,C3,R4,M4,C4,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,T3),
                    possibilities(R3,M3,T3,C3,T4), combineList(T3,T4,T5),
                    possibilities(R4,M4,T5,C4,T6), combineList(T5,T6,Result).
conditions5(R1,M1,C1,R2,M2,C2,R3,M3,C3,R4,M4,C4,R5,M5,C5,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,T3),
                    possibilities(R3,M3,T3,C3,T4), combineList(T3,T4,T5),
                    possibilities(R4,M4,T5,C4,T6), combineList(T5,T6,T7),
                    possibilities(R5,M5,T7,C5,T8), combineList(T7,T8,Result).
conditions6(R1,M1,C1,R2,M2,C2,R3,M3,C3,R4,M4,C4,R5,M5,C5,R6,M6,C6,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,T3),
                    possibilities(R3,M3,T3,C3,T4), combineList(T3,T4,T5),
                    possibilities(R4,M4,T5,C4,T6), combineList(T5,T6,T7),
                    possibilities(R5,M5,T7,C5,T8), combineList(T7,T8,T9),
                    possibilities(R6,M6,T9,C6,T10), combineList(T9,T10,Result).
conditions7(R1,M1,C1,R2,M2,C2,R3,M3,C3,R4,M4,C4,R5,M5,C5,R6,M6,C6,R7,M7,C7,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,T3),
                    possibilities(R3,M3,T3,C3,T4), combineList(T3,T4,T5),
                    possibilities(R4,M4,T5,C4,T6), combineList(T5,T6,T7),
                    possibilities(R5,M5,T7,C5,T8), combineList(T7,T8,T9),
                    possibilities(R6,M6,T9,C6,T10), combineList(T9,T10,T11),
                    possibilities(R7,M7,T11,C7,T12), combineList(T11,T12,Result).
conditions8(R1,M1,C1,R2,M2,C2,R3,M3,C3,R4,M4,C4,R5,M5,C5,R6,M6,C6,R7,M7,C7,R8,M8,C8,Result):-
                    possibilities(R1,M1,[],C1,T1),
                    possibilities(R2,M2,T1,C2,T2), combineList(T1,T2,T3),
                    possibilities(R3,M3,T3,C3,T4), combineList(T3,T4,T5),
                    possibilities(R4,M4,T5,C4,T6), combineList(T5,T6,T7),
                    possibilities(R5,M5,T7,C5,T8), combineList(T7,T8,T9),
                    possibilities(R6,M6,T9,C6,T10), combineList(T9,T10,T11),
                    possibilities(R7,M7,T11,C7,T12), combineList(T11,T12,T13),
                    possibilities(R8,M8,T12,C8,T14), combineList(T13,T14,Result).
conditions9(R1,_,_,R2,_,_,R3,_,_,R4,_,_,R5,_,_,R6,_,_,R7,_,_,R8,_,_,R9,_,_,[R1,R2,R3,R4,R5,R6,R7,R8,R9]).







% A few examples of application
% -------------

%game1(Result):-possibilities(-1,2,[],subtraction,X),possibilities(2,2,X,division,Y), combineList(X,Y,T1),possibilities(16,3,T1,sum,Z), combineList(T1,Z,T2), possibilities(72,2,T2,multiplication,W), combineList(T2,W,Result).
%game1(Result):- conditions4(-1,2,subtraction,2,2,division,16,3,sum,72,2,multiplication,Result).
%game1(Result):- conditions4(-1,2,-,2,2,/,16,3,+,72,2,*,Result).

%game2(Result):-
%            possibilities(-1,2,[],subtraction,X),
%            possibilities(2,2,X,division,Y), combineList(X,Y,Result).

%constructGame3(X,Y,Z,W,R):-conditions4(X,2,-,Y,1,/,Z,2,+,W,4,*,R).


end_of_file.







