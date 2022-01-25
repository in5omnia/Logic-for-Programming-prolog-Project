%OMD FUNCIONA
% 2.1 - extrai_ilhas_linha/3
% Da a lista ordenada (da esquerda para a direita) de ilhas da linha do puzzle. 
% ____________________________________________________________________

extrai_ilhas_linha(NL, Linha_puzzle, Ilhas) :-
    extrai_ilhas_linha(NL, Linha_puzzle, 1, [], Ilhas), !.

extrai_ilhas_linha(_, [], _, Ilhas_Aux, Ilhas_Aux).


extrai_ilhas_linha(NL, [P|R], Ind, Ilhas_Aux, Ilhas) :-
    ( \==(P, 0) -> append(Ilhas_Aux, [ilha(P, (NL, Ind))], Ilhas_Aux2);
    Ilhas_Aux2 = Ilhas_Aux), 
    Index is Ind + 1,
    extrai_ilhas_linha(NL, R, Index, Ilhas_Aux2, Ilhas).

% ____________________________________________________________________
% 2.2 - ilhas/2
% Da a lista ordenada (da esquerda para a direita, e de cima para baixo) de 
% ilhas do puzzle.
% ____________________________________________________________________

ilhas([], []).

ilhas(Puzzle, Ilhas) :- 
    ilhas(Puzzle, 1, [], Ilhas).

ilhas([], _, Ilhas_Aux, Ilhas_Aux).

ilhas([P|R], NL, Ilhas_Aux, Ilhas) :-   
    extrai_ilhas_linha(NL, P, Ilhas_L), 
    append(Ilhas_Aux, Ilhas_L, Ilhas_Aux2), 
    % juntar a lista de ilhas da linha Nl ah lista de ilhas ja existente
    NL_2 is NL + 1,
    ilhas(R, NL_2, Ilhas_Aux2, Ilhas).


% ____________________________________________________________________
% 2.3 - vizinhas/3
% Da a lista ordenada (de cima para baixo e da esquerda para a direita) de ilhas que 
% sao vizinhas de Ilha.
% ____________________________________________________________________


vizinhas([], _, []).

vizinhas(Ilhas, Ilha, Vizinhas) :- 
    exclude(==(Ilha), Ilhas, Ilhas_Sem_Propria),   
    % retira a propria Ilha da lista das possiveis vizinhas.
    Ilha =.. [ilha, _, (L, C)],
    findall(I, (member(I, Ilhas_Sem_Propria), I =..[ilha, _, (L,_)]), Mesma_Linha),    
    % lista de ilhas na mesma linha.
    findall(I, (member(I, Ilhas_Sem_Propria), I =..[ilha, _, (_,C)]), Mesma_Coluna),   
    % lista de ilhas na mesma coluna.
    length(Mesma_Linha, LenL),
    % se ha 0 ou apenas 1 ilha na mesma linha, eh essa a lista das vizinhas na linha.
    (LenL =< 1 -> Vizinhas_Linha = Mesma_Linha; 
    (findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col < C), Esquerda),   
    % lista de ilhas na mesma linha ah esquerda. 
    length(Esquerda, LenE),
    % se ha 0 ou 1 vizinha ah esquerda, nao eh preciso escolher.
    (LenE =< 1 -> Lst_Viz_Esq = Esquerda; 
    % a ultima ilha de Esquerda (com a > coluna) esta mais perto da Ilha e eh sua vizinha. 
    (nth1(LenE, Esquerda, Viz_Esq),   
    Lst_Viz_Esq = [Viz_Esq])),
    findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col > C), Direita),   
    % Direita = lista de ilhas na mesma linha ah direita.
    % nao havendo ilhas ah direita, a vizinha da esquerda eh a unica na mesma linha.
    (Direita = [] -> Vizinhas_Linha = Lst_Viz_Esq;
    % a primeira vizinha da direita tem < coluna e esta + perto da Ilha.
    (nth1(1, Direita, Viz_Dir), 
    append(Lst_Viz_Esq, [Viz_Dir], Vizinhas_Linha))))),
    % nao havendo ilhas na mesma coluna, as unicas vizinhas serao as da mesma linha.
    (Mesma_Coluna = [] -> Vizinhas = Vizinhas_Linha; 
    (findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin < L), Acima),   
    % Acima = lista de ilhas na mesma coluna acima 
    length(Acima, LenA),
    % havendo apenas 0 ou 1 ilha acima, sera essa a lista da vizinha acima (ou lista 
    % vazia) que se une ahs vizinhas da mesma linha.
    (LenA =< 1 -> append(Acima, Vizinhas_Linha, Viz_Sem_Abaixo); 
    nth1(LenA, Acima, Viz_Acima),   
    % a ilha acima com > linha (a ultima da lista) esta + perto da Ilha e eh sua vizinha.
    append([Viz_Acima], Vizinhas_Linha, Viz_Sem_Abaixo)),
    findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin > L), Abaixo),   
    % Abaixo = lista de ilhas, na mesma coluna, abaixo.
    % nao havendo ilhas abaixo, as vizinhas estao ja definidas em Viz_Sem_Abaixo.
    (Abaixo = [] -> Vizinhas = Viz_Sem_Abaixo;
    % a primeira ilha abaixo tem < linha e logo esta + perto da Ilha e eh sua vizinha.
    (nth1(1, Abaixo, Viz_Abaixo),
    append(Viz_Sem_Abaixo, [Viz_Abaixo], Vizinhas))))). 


% ____________________________________________________________________
% 2.4 - estado/2
% Da a lista ordenada (estado) de entradas referentes a cada uma das ilhas, tal que 
% Entrada = [ilha, lista de vizinhas, lista de pontes], sendo a lista de pontes,
% inicialmente, vazia.
% ____________________________________________________________________

estado([], []).
estado(Ilhas, Estado) :- estado(Ilhas, Ilhas, [], Estado).
estado([], _, Estado, Estado).
estado([P|R], Ilhas, Acc_Estado, Estado) :-
    vizinhas(Ilhas, P, Vizinhas),
    append([P], [Vizinhas], Quase_Entrada),
    append(Quase_Entrada , [[]], Entrada),
    append(Acc_Estado, [Entrada], Acc_Estado2),
    estado(R, Ilhas, Acc_Estado2, Estado).

% ____________________________________________________________________
% 2.5 - posicoes_entre/3
% Da a lista ordenada de posicoes entre Pos1 e Pos2, exclusive, se estas pertencerem  
% ah mesma linha ou coluna. Caso contrario, da false.
% ____________________________________________________________________

% funcao auxiliar 
posicoes_entre_aux(CL1, CL2, CL_Comum, N, Posicoes) :-
    findall(CL_P, (between(CL1, CL2, CL_P), CL_P \== CL1, CL_P \== CL2), Lista),
    (N == 0 -> findall(P, (P = (CL_Comum, CL_P), member(CL_P, Lista)), Posicoes); 
    % posicoes na mesma linha (N=0), entre as colunas de Pos1 e Pos2
    findall(P, (P = (CL_P, CL_Comum), member(CL_P, Lista)), Posicoes)). 
    % posicoes na mesma coluna, entre as linhas de Pos1 e Pos2



% posicoes_entre(Pos, Pos, []).     % nao sei se deve dar falso ou []---------------------------------------------


posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 \== Pos2,
    Pos1 = (L1, C1),
    Pos2 = (L2, C2),
    % tem ou linhas ou colunas =, mas nao ambas, pois posicoes =
    % nao teriam nenhumas posicoes entre elas
    (L1 == L2; C1 == C2),  
    ( L1 == L2 -> (C1 < C2 -> posicoes_entre_aux(C1, C2, L1, 0, Posicoes);   
    posicoes_entre_aux(C2, C1, L1, 0, Posicoes));    % se C1 > C2
    % se L1 \== L2, entao C1 == C2 necessariamente
    (L1 < L2 -> posicoes_entre_aux(L1, L2, C1, 1, Posicoes);  
    (posicoes_entre_aux(L2, L1, C1, 1, Posicoes)))).    % se L1 > L2

% ____________________________________________________________________
% 2.6 - cria_ponte/3
% Cria uma ponte entre as posicoes Pos1 e Pos2, tal que ponte = ponte(Pos1, Pos2).
% ____________________________________________________________________

cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (L1, C1),
    Pos2 = (L2, C2),
    ((L1 < L2; C1 < C2) -> Ponte =.. [ponte, Pos1, Pos2];   % ordenar posicoes
    Ponte =.. [ponte, Pos2, Pos1]).

% ____________________________________________________________________
% 2.7 - caminho_livre/5
% Informa se Pos1 e Pos2 continuam a ser vizinhas apos adicionar ponte(Pos1, Pos2).
% ____________________________________________________________________


caminho_livre(_, _, Posicoes, ilha(_, Pos_Ilha), ilha(_, Pos_Viz)) :-
    posicoes_entre(Pos_Ilha, Pos_Viz, Pos_entre_Vizinhas),
    caminho_livre_aux(Pos_entre_Vizinhas, Posicoes).  
    % se o caminho entre as ilhas vizinhas for = ao da ponte, entao a ponte eh entre elas

caminho_livre_aux(Posicoes, Posicoes). 
% se o caminho entre as ilhas vizinhas for = ao da ponte, entao a ponte eh entre elas

caminho_livre_aux([], _). 

caminho_livre_aux(Pos_entre_Vizinhas, Posicoes) :-
    findall(Pos, (member(Pos, Posicoes), \+ member(Pos, Pos_entre_Vizinhas)), Posicoes_livres),
    Posicoes_livres == Posicoes.
    % se Posicoes nao se alterou, entao nao coincidia com a posicao P 

    %exclude(==(P), Posicoes, Posicoes2),
    %Posicoes2 == Posicoes,  
    %% se Posicoes nao se alterou, entao nao coincidia com a posicao P 
    %caminho_livre_aux(R, Posicoes).

% ____________________________________________________________________
% 2.8 - actualiza_vizinhas_entrada/5
% Da uma entrada com a lista de ilhas vizinhas atualizada, apos remover as que deixaram 
% de ser vizinhas com a adicao de uma ponte.
% ____________________________________________________________________


actualiza_vizinhas_entrada(_, _, Posicoes, [Ilha, Vizinhas, Lista_Pontes], 
        [Ilha, Vizinhas_Atualizadas, Lista_Pontes]) :-
    findall(Viz, (member(Viz, Vizinhas), caminho_livre(_, _, Posicoes, Ilha, Viz)), Vizinhas_Atualizadas).


% ____________________________________________________________________
% 2.9 - actualiza_vizinhas_apos_pontes/4
% Da o estado apos atualizar as ilhas vizinhas de cada uma das suas entradas depois da 
% adicao de uma ponte entre Pos1 e Pos2.
% ____________________________________________________________________


actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_Estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes_entre), 
    !,
    maplist(actualiza_vizinhas_entrada(_, _, Posicoes_entre), Estado, Novo_Estado).


% ____________________________________________________________________
% 2.10 - ilhas_terminadas/2
% Da a lista de ilhas terminadas do estado, ou seja, as ilhas que tem todas as pontes 
% associadas.
% ____________________________________________________________________

ilhas_terminadas(Estado, Ilhas_term) :-
    findall(Entrada, (member(Entrada, Estado), Entrada = [ilha(N_pontes, _), _, Pontes], 
        N_pontes \== 'X', length(Pontes, N_pontes)), Entradas_Ilhas_term),
    maplist(nth1(1), Entradas_Ilhas_term, Ilhas_term).

% ____________________________________________________________________
% 2.11 - tira_ilhas_terminadas_entrada/3
% Da a entrada resultante de remover as ilhas terminadas da sua lista de ilhas vizinhas.
% ____________________________________________________________________

tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Viz, Pont], [Ilha, Novas_Viz, Pont]) :-  
    findall(Ilha_nao_term, 
        (member(Ilha_nao_term, Viz), \+ member(Ilha_nao_term, Ilhas_term)),
        Novas_Viz).


% ____________________________________________________________________
% 2.12 - tira_ilhas_terminadas/3
% Da o Novo_estado, resultante de remover as ilhas terminadas de cada entrada do Estado.
% ____________________________________________________________________

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

% ____________________________________________________________________
% 2.13 - marca_ilhas_terminadas_entrada/3
% Da a entrada apos substituir o numero de pontes da sua ilha por 'X', se esta pertencer 
% ahs ilhas terminadas (Ilhas_term), caso contrario a entrada mantem-se igual.
% ____________________________________________________________________


marca_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Viz, Pont], [Ilha_com_X, Viz, Pont]) :- 
    member(Ilha, Ilhas_term), 
    Ilha = ilha(_, (L,C)), 
    Ilha_com_X = ilha('X', (L,C)).

marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Entrada) :- 
    Entrada = [Ilha, _, _], 
    \+ member(Ilha, Ilhas_term).

% ____________________________________________________________________
% 2.14 - marca_ilhas_terminadas/3
% Da o Novo_estado resultante de aplicar o predicado  
% marca_ilhas_terminadas_entrada ao Estado.
% ____________________________________________________________________


marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


% ____________________________________________________________________
% 2.15 - trata_ilhas_terminadas/2
% Da o Novo_estado, resultante de aplicar os predicados tira_ilhas_terminadas e 
% marca_ilhas_terminadas a Estado.
% ____________________________________________________________________


trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado, Ilhas_term),
    tira_ilhas_terminadas(Estado, Ilhas_term, Quase_novo_estado),
    marca_ilhas_terminadas(Quase_novo_estado, Ilhas_term, Novo_estado).
    

% ____________________________________________________________________
% 2.16 - junta_pontes/5
% Da o Novo_estado, obtido de Estado por adicao de Num_pontes pontes entre Ilha1 e Ilha2.
% ____________________________________________________________________


junta_pontes(Estado, Num_pontes, ilha(NumP1, Pos1), ilha(NumP2, Pos2), Novo_estado) :-
    cria_ponte(Pos1, Pos2, Ponte),   
    length(Lista_pontes, Num_pontes), maplist(=(Ponte), Lista_pontes),
    maplist(adiciona_pontes(Lista_pontes, ilha(NumP1, Pos1), ilha(NumP2, Pos2)), 
        Estado, Estado_com_pontes),
    actualiza_vizinhas_apos_pontes(Estado_com_pontes, Pos1, Pos2, Estado_atualizado),
    trata_ilhas_terminadas(Estado_atualizado, Novo_estado).


adiciona_pontes(Lst_pontes, Ilha1, Ilha2, [Ilha, Viz, Pontes_inicio], [Ilha, Viz, Pontes_fim]) :- 
    ((Ilha = Ilha1; Ilha = Ilha2) -> append(Pontes_inicio, Lst_pontes, Pontes_fim);
    Pontes_fim = Pontes_inicio).    



% funcao auxiliar para posicoes


