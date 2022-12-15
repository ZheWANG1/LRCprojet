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

concept_tbox([]).
concept_tbox([(C1,C2)|O]) :- cnamena(C1), concept(C2), concept_tbox(O), !.
concept_abox([]).
concept_abox([(I,C)|O]) :- instance(I), concept(C), inst(I,C), concept_abox(O), !.
concept_abox([(I1,I2,R)|O]) :- instance(I1), instance(I2), role(R), instR(I1,R,I2), concept_abox(O), !.

% Tester si ce concept est auto-référent.
autoref(C, C).
autoref(and(C1,C2), C) :- autoref(C1, C), autoref(C2, C), !.
autoref(or(C1,C2), C) :- autoref(C1, C), autoref(C2, C), !.
autoref(not(C), C) :- autoref(C, C), !.
autoref(some(R,C1), C) :- autoref(C1, C), !.
autoref(all(R,C1), C) :- autoref(C1, C), !.

remplace(C, C) :- cnamea(C), !.
remplace(C, E) :- equiv(C, D), remplace(D, E) ,!.
remplace(not(C1), not(C2)) :- 	remplace(C1, C2), !.
remplace(or(C1, C2), or(D1, D2)) :- remplace(C1, D1), remplace(C2, D2), !.
remplace(and(C1, C2), and(D1, D2)) :- remplace(C1, D1), remplace(C2, D2), !.
remplace(some(R, C1), some(R, C2)) :- 	remplace(C1, C2), !.
remplace(all(R, C1), all(R, C2)) :- remplace(C1, C2), !.

traitement_Tbox([],[]).
traitement_Tbox([(C1,C2)|O],[(C1,C3)|O1]) :- remplace(C2,C4), nnf(C4,C3), traitement_Tbox(O,O1), !.
traitement_Abox([],[]).
traitement_Abox([(I,C)|O],[(I,C1)|O1]) :- remplace(C,C2), nnf(C2,C1), traitement_Abox(O,O1), !.
traitement_Abox([(I1,I2,R)|O],[(I1,I2,R)|O1]) :- traitement_Abox(O,O1), !.

% ----------------------------------------------------------------
% Partie 2 - Saisie de la proposition à démontrer
% ----------------------------------------------------------------

programme :-
    premiere_etape(Tbox,Abi,Abr),
    deuxieme_etape(Abi,Abi1,Tbox),
    troisieme_etape(Abi1,Abr).

premiere_etape(Tbox,Abi,Abr) :- 
    setof((C, D), equiv(C, D), T), traitement_Tbox(T,Tbox),
    setof((I, C), inst(I, C), Ai), traitement_Abox(Ai,Abi),
    setof((I1, I2, R), instR(I1, I2, R), Ar), traitement_Abox(Ar,Abr), !.

deuxieme_etape(Abi,Abi1,Tbox) :- saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :- nl,
    write('Entrez le numero du type de proposition que vous voulez demontrer :'),nl,
    write('1 Une instance donnee appartient a un concept donne.'),nl,
    write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).'),nl,
    read(R),
    suite(R,Abi,Abi1,Tbox).
suite(1,Abi,Abi1,Tbox) :- acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :- acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(R,Abi,Abi1,Tbox) :- nl,write('Cette reponse est incorrecte.'),nl,
saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

acquisition_prop_type1(Abi,Abi1,Tbox):- nl,
    write('Entrez le nom de l"instance :'),nl,
    read(I),
    write('Entrez le nom du concept :'),nl,
    read(C),
    remplace(C, C1), nnf(not(C1),C2), concat(Abi,[I,C2],Abi1), !.

acquisition_prop_type2(Abi,Abi1,Tbox):- nl,
    write('Entrez le nom du premier concept :'),nl,
    read(C1),
    write('Entrez le nom du second concept :'),nl,
    read(C2),
    remplace(C1, C3), remplace(C2, C4), nnf(not(C3),C5), nnf(not(C4),C6),
    concat(Abi,[C5,C6],Abi1), !.

% ----------------------------------------------------------------
% Partie 3 - Démonstration de la proposition
% ----------------------------------------------------------------

troisieme_etape(Abi,Abr):-
    tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
    resolution(Lie,Lpt,Li,Lu,Ls,Abr),
    nl,write('Youpiiiiii, on a demontre la proposition initiale !!!').

compteur(1).

tri_Abox([],[],[],[],[],[]).

tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

resolution(Lie,Lpt,Li,Lu,Ls,Abr).

complete_some(Lie,Lpt,Li,Lu,Ls,Abr).
transformation_and(Lie,Lpt,Li,Lu,Ls,Abr).
deduction_all(Lie,Lpt,Li,Lu,Ls,Abr).
transformation_or(Lie,Lpt,Li,Lu,Ls,Abr).
evolue(A, Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1).
affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2, Lpt2, Li2, Lu2, Abr2).
