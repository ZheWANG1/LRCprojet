% Include
:-[init].
:-[utils].

% ----------------------------------------------------------------
% Partie 1 - Etape préliminaire de vérification et de mise en forme de la Tbox et de la Abox
% ----------------------------------------------------------------

% Vérifier la correction sémantique et syntaxique des expressions des concepts atomiques ou complexes.
concept([C|_]) :- setof(X, cnamea(X), L), member(C, L), !.
concept([C|_]) :- setof(X, cnamena(X), L), member(C, L), !.
concept([and(C1,C2)|_]) :- concept([C1]), concept([C2]), !.
concept([or(C1,C2)|_]) :- concept([C1]), concept([C2]), !.
concept([not(C)|_]) :- concept([C]), !.
concept([some(R,C)|_]) :- rname(R), concept([C]), !.
concept([all(R,C)|_]) :- rname(R), concept([C]), !.

% Tester si ce concept est auto-référent.



traitement_Tbox(Tbox) :- setof((X, Y), equiv(X, Y), Tbox), !.
traitement_Abox(Abi, Abr) :- aboxInst(Abi), aboxRole(Abr), !.
aboxInst(Abi) :- setof((X, Y), inst(X, Y), Abi), !.
aboxRole(Abr) :- setof((X, Y, Z), instR(X, Y, Z), Abr), !.


% tbox([]).
% tbox(L) :- setof((X,Y), equiv(X,Y), L).
% tbox([C1|C2]) :- setof((X,Y), equiv(X,Y), L), member(C1, L), tbox(C2).

