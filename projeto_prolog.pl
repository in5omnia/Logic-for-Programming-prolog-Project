%OMD FUNCIONA
% 2.1

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

% ____________________________________________________________________
% 2.2
% ____________________________________________________________________

ilhas([], []).

ilhas(Puz, Ilhas) :- 
    ilhas(Puz, 1, [], Ilhas).

ilhas([], _, Ilhas_Aux, Ilhas_Aux).

ilhas([P|R], NL, Ilhas_Aux, Ilhas) :-   
    extrai_ilhas_linha(NL, P, Ilhas_L), 
    append(Ilhas_Aux, Ilhas_L, Ilhas_Aux2), % juntar a lista de ilhas da linha Nl ah lista de ilhas ja existente
    NL_2 is NL + 1,
    ilhas(R, NL_2, Ilhas_Aux2, Ilhas).

%length(Puz, Len), extrai_ilhas_linha(1, Linha_escrita, Ilhas), !,
%NL is N + 1,
% NL =< Len,
%extrai_ilhas_linha(NL, Linha_escrita, Ilhas).

%append(extrai_ilhas_linha(NL, P, Ilhas_L), Ilhas_Aux, Ilhas_Aux2),

%ilhas([[2, 0, 0, 0, 0, 0, 0], [0, 0, 2, 0, 4, 0, 2], [0, 0, 0, 0, 0, 0, 0]], L).

% ____________________________________________________________________
% 2.3 
% ____________________________________________________________________

% se encontrarem na mesma linha ou mesma coluna;
% entre elas nao existir outra ilha;
% entre elas nao existir uma ponte que una outras duas ilhas quaisquer.

vizinhas([], _, []).

vizinhas(Ilhas, Ilha, Vizinhas) :- 
    exclude(==(Ilha), Ilhas, Ilhas2),   % retirar a propria Ilha da lista das possiveis vizinhas
    Ilha =.. [ilha, _, (L, C)],
    findall(I, (member(I, Ilhas2), I =..[ilha, _, (L,_)]), Mesma_Linha),    % lista de ilhas na mesma linha
    findall(I, (member(I, Ilhas2), I =..[ilha, _, (_,C)]), Mesma_Coluna),   % lista de ilhas na mesma coluna
    length(Mesma_Linha, LenL),
    %length(Mesma_Coluna, LenC), % unecessary
    (LenL =< 1 -> Vizinhas_Linha = Mesma_Linha; 
    (findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col < C), Esquerda),   % Esquerda = lista de ilhas na mesma linha ah esquerda 
    length(Esquerda, LenE),
    % yoooooooooooo
    (LenE =< 1 -> Viz_Sem_Dir = Esquerda; 
    (nth1(LenE, Esquerda, Viz_Esq),   % if LenA =< 1
    Viz_Sem_Dir = [Viz_Esq])),
    findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col > C), Direita),    % Direita = lista de ilhas na mesma linha ah direita 
    (Direita = [] -> Vizinhas_Linha = Viz_Sem_Dir;
    (nth1(1, Direita, Viz_Dir),
    append(Viz_Sem_Dir, [Viz_Dir], Vizinhas_Linha))))),
    % lista de ilhas na mesma coluna
    %LenC
    (Mesma_Coluna = [] -> Vizinhas = Vizinhas_Linha; 
    (findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin < L), Acima),   % Cima = lista de ilhas na mesma coluna acima 
    length(Acima, LenA),
    (LenA =< 1 -> append(Acima, Vizinhas_Linha, Viz_Sem_Abaixo); 
    nth1(LenA, Acima, Viz_Acima),   % if LenA =< 1
    append([Viz_Acima], Vizinhas_Linha, Viz_Sem_Abaixo)),
    findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin > L), Abaixo),   % Abaixo = lista de ilhas na mesma coluna abaixo
    (Abaixo = [] -> Vizinhas = Viz_Sem_Abaixo;
    (nth1(1, Abaixo, Viz_Abaixo),
    append(Viz_Sem_Abaixo, [Viz_Abaixo], Vizinhas))))).

%maplist(I_C - C, )    % OU =:=???????    % o q tiver menor dist
% agora para as colunas
% NAO ESQUECER DE POR NA ORDEM CIMA ESQ DIR BAIXO
%VER SE DA PARA VAZIO


%Viz_Dir
%Viz_Esq

%obj: [Viz_Cima, Viz_Esq, Viz_Dir, Viz_Baixo]
% === append([Viz_Cima], Vizinhas_Linha, Viz_Sem_Esq), append(Viz,S_Esq, [Viz_Baixo], Vizinhas) 

% Ilhas = [ilha(2,(1,3)),ilha(1,(3,1)),ilha(6,(3,3)),ilha(1,(3,5)),ilha(2,(5,3))], Ilha = ilha(6, (3,3)),  exclude(==(Ilha), Ilhas, Ilhas2), Ilha =.. [ilha, _, (L, C)], findall(I, (member(I, Ilhas2), I =..[ilha, _, (L,_)]), Mesma_Linha), findall(I, (member(I, Ilhas2), I =..[ilha, _, (_,C)]), Mesma_Coluna), length(Mesma_Linha, LenL), (LenL =< 1 -> Vizinhas_Linha = Mesma_Linha; findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col < C), Esquerda)

%Ilhas = [ilha(2,(1,3)),ilha(1,(3,1)),ilha(6,(3,3)),ilha(2, (3, 2)),ilha(1,(3,5)),ilha(2,(5,3))], vizinhas(Ilhas, ilha(1, (3, 1)), Vizinhas), writeln(Vizinhas), writeln(false).

% Ilhas = [ilha(2,(1,3)),ilha(1,(3,1)),ilha(6,(3,3)),ilha(1,(3,5)),ilha(2, (3, 2)),ilha(2,(5,3))], Ilha = ilha(6, (3,3)),  exclude(==(Ilha), Ilhas, Ilhas2), Ilha =.. [ilha, _, (L, C)], findall(I, (member(I, Ilhas2), I =..[ilha, _, (L,_)]), Mesma_Linha), findall(I, (member(I, Ilhas2), I =..[ilha, _, (_,C)]), Mesma_Coluna), length(Mesma_Linha, LenL), (LenL =< 1 -> Vizinhas_Linha = Mesma_Linha; findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col < C), Esquerda), length(Esquerda, LenE), nth1(LenE, Esquerda, Viz_Esq), findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col > C), Direita), nth1(1, Direita, Viz_Dir), append([Viz_Esq], [Viz_Dir], Vizinhas_Linha)), (Mesma_Coluna = []  -> Vizinhas = Vizinhas_Linha; findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin < L), Acima), length(Acima, LenA), nth1(LenA, Acima, Viz_Acima), findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin > L), Abaixo), nth1(1, Abaixo, Viz_Abaixo), append([Viz_Acima], Vizinhas_Linha, Viz_Sem_Esq), append(Viz_Sem_Esq, [Viz_Abaixo], Vizinhas)).

 %(18) 5<1 ? creep
%^ % Exit: (12) findall(_9894, user:(member(_9894, [ilha(6,  (3, 3)), ilha(1,  (3, 5))]), _9894=..[ilha, _41300,  (_41312, _41314)], _41314<1), []) ? creep
   %Call: (12) length([], _51994) ? creep
  % Exit: (12) length([], 0) ? creep
  % Call: (12) lists:nth1(0, [], _53508) ? creep
  %5 Fail: (12) lists:nth1(0, [], _53508) ? creep
  % Fail: (11) vizinhas([ilha(2,  (1, 3)), ilha(1,  (3, 1)), ilha(6,  (3, 3)), ilha(1,  (3, 5)), ilha(2,  (5, 3))], ilha(1,  (3, 1)), _22) ? creep
%false.




    %nth1(LenE, Esquerda, Viz_Esq),
    %findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col > C), Direita),   % Direita = lista de ilhas na mesma linha ah direita 
    %nth1(1, Direita, Viz_Dir),    % vou acreditar que as ilhas vem ordenadas!!!!
%append([Viz_Esq], [Viz_Dir], Vizinhas_Linha)))
