% Include
:-[init].
:-[utils].

% ----------------------------------------------------------------
% Partie 1 - Etape préliminaire de vérification et de mise en forme de la Tbox et de la Abox
% ----------------------------------------------------------------

% Vérifier la correction sémantique et syntaxique des expressions des concepts atomiques ou complexes.
concept(C) :- cnamea(C), !.
concept(C) :- cnamena(C), !.
instance(I) :- iname(I), !.
role(R) :- rname(R), !.

concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(not(C)) :- concept(C), !.
concept(some(R,C)) :- role(R), concept(C), !.
concept(all(R,C)) :- role(R), concept(C), !.

concept([C|_]) :- setof(X, cnamea(X), L), member(C, L), !.
concept([C|_]) :- setof(X, cnamena(X), L), member(C, L), !.
concept([and(C1,C2)|_]) :- concept([C1]), concept([C2]), !.
concept([or(C1,C2)|_]) :- concept([C1]), concept([C2]), !.
concept([not(C)|_]) :- concept([C]), !.
concept([some(R,C)|_]) :- rname(R), concept([C]), !.
concept([all(R,C)|_]) :- rname(R), concept([C]), !.

% Tester si ce concept est auto-référent.
autoref(C, C).
autoref(and(C1,C2), C) :- autoref(C1, C), autoref(C2, C), !.
autoref(or(C1,C2), C) :- autoref(C1, C), autoref(C2, C), !.
autoref(not(C), C) :- autoref(C, C), !.
autoref(some(R,C1), C) :- autoref(C1, C), !.
autoref(all(R,C1), C) :- autoref(C1, C), !.

traitement_Tbox(Tbox) :- setof((X, Y), equiv(X, Y), Tbox), !.
traitement_Abox(Abi, Abr) :- aboxInst(Abi), aboxRole(Abr), !.
aboxInst(Abi) :- setof((X, Y), inst(X, Y), Abi), !.
aboxRole(Abr) :- setof((X, Y, Z), instR(X, Y, Z), Abr), !.

% tbox([]).
% tbox(L) :- setof((X,Y), equiv(X,Y), L).
% tbox([C1|C2]) :- setof((X,Y), equiv(X,Y), L), member(C1, L), tbox(C2).

% ----------------------------------------------------------------
% Partie 2 - Saisie de la proposition à démontrer
% ----------------------------------------------------------------

programme :-
    premiere_etape(Tbox,Abi,Abr),
    deuxieme_etape(Abi,Abi1,Tbox),
    troisieme_etape(Abi1,Abr).

premiere_etape(Tbox,Abi,Abr).

deuxieme_etape(Abi,Abi1,Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).
saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :- nl,write('Entrez le numero du type de proposition que vous voulez demontrer :'),nl, write('1 Une instance donnee appartient a un concept donne.'),nl, write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).’),nl, read(R), suite(R,Abi,Abi1,Tbox).
suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :- nl,write('Cette reponse est incorrecte.'),nl,
saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

troisieme_etape(Abi1,Abr).













% ----------------------------------------------------------------
% Partie 3 - Démonstration de la proposition
% ----------------------------------------------------------------
