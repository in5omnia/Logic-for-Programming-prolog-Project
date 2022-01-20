%OMD FUNCIONA
extrai_ilhas_linha(NL, Linha_escrita, Ilhas) :-
    extrai_ilhas_linha(NL, Linha_escrita, 1, [], Ilhas), !.

extrai_ilhas_linha(_, [], _, Ilhas_Aux, Ilhas_Aux).
%extrai_ilhas_linha(_, [], 1, Ilhas_Aux, Ilhas_Aux).

extrai_ilhas_linha(NL, [P|R], Ind, Ilhas_Aux, Ilhas) :-
    ( \==(P, 0) -> append(Ilhas_Aux, [ilha(P, (NL, Ind))], Ilhas_Aux2), Index is Ind + 1, extrai_ilhas_linha(NL, R, Index, Ilhas_Aux2, Ilhas) ; 
    Index is Ind + 1,
    extrai_ilhas_linha(NL, R, Index, Ilhas_Aux, Ilhas)).


%?- extrai_ilhas_linha(1,[1,0,0,0,8],X).
%X = [ilha(8,  (1, 1+1+1+1+1))] 


%length(Linha_escrita, Comp),
%extrai_ilhas_linha(NL, [P|R], Ind, Ilhas_Aux, Ilhas) :-
    %( \==(P, 0) -> append(Ilhas_Aux, [ilha(P, (NL, Ind))], Ilhas_Aux2) ; append(Ilhas_Aux, [], Ilhas_Aux2)),
    %Index is Ind + 1,
    %extrai_ilhas_linha(NL, R, Index, Ilhas_Aux2, Ilhas).



ilhas([], []).

ilhas(Puz, Ilhas) :- 
    ilhas(Puz, 1, [], Ilhas).

ilhas([], _, Ilhas_Aux, Ilhas_Aux).

ilhas([P|R], NL, Ilhas_Aux, Ilhas) :-   
    extrai_ilhas_linha(NL, P, Ilhas_L), 
    append(Ilhas_Aux, Ilhas_L, Ilhas_Aux2), % juntar a lista de ilhas da linha Nl ah lista de ilhas ja existente
    NL_2 is NL + 1,
    ilhas(R, NL_2, Ilhas_Aux2, Ilhas).

%lenght(Puz, Len), extrai_ilhas_linha(1, Linha_escrita, Ilhas), !,
%NL is N + 1,
% NL =< Len,
%extrai_ilhas_linha(NL, Linha_escrita, Ilhas).

%append(extrai_ilhas_linha(NL, P, Ilhas_L), Ilhas_Aux, Ilhas_Aux2),

%ilhas([[2, 0, 0, 0, 0, 0, 0], [0, 0, 2, 0, 4, 0, 2], [0, 0, 0, 0, 0, 0, 0]], L).
