%OMD FUNCIONA
% 2.1

extrai_ilhas_linha(NL, Linha_escrita, Ilhas) :-
    extrai_ilhas_linha(NL, Linha_escrita, 1, [], Ilhas), !.

extrai_ilhas_linha(_, [], _, Ilhas_Aux, Ilhas_Aux).


extrai_ilhas_linha(NL, [P|R], Ind, Ilhas_Aux, Ilhas) :-
    ( \==(P, 0) -> append(Ilhas_Aux, [ilha(P, (NL, Ind))], Ilhas_Aux2);
    Ilhas_Aux2 = Ilhas_Aux), 
    Index is Ind + 1,
    extrai_ilhas_linha(NL, R, Index, Ilhas_Aux2, Ilhas).

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


% ____________________________________________________________________
% 2.3 
% ____________________________________________________________________


vizinhas([], _, []).

vizinhas(Ilhas, Ilha, Vizinhas) :- 
    exclude(==(Ilha), Ilhas, Ilhas_Sem_Propria),   % retira a propria Ilha da lista das possiveis vizinhas
    Ilha =.. [ilha, _, (L, C)],
    findall(I, (member(I, Ilhas_Sem_Propria), I =..[ilha, _, (L,_)]), Mesma_Linha),    % lista de ilhas na mesma linha
    findall(I, (member(I, Ilhas_Sem_Propria), I =..[ilha, _, (_,C)]), Mesma_Coluna),   % lista de ilhas na mesma coluna
    length(Mesma_Linha, LenL),
    % se ha 0 ou apenas 1 ilha na mesma linha, eh essa a lista das vizinhas na linha
    (LenL =< 1 -> Vizinhas_Linha = Mesma_Linha; 
    (findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col < C), Esquerda),   % lista de ilhas na mesma linha ah esquerda 
    length(Esquerda, LenE),
    % se ha 0 ou 1 vizinha ah esquerda, nao eh preciso escolher
    (LenE =< 1 -> Lst_Viz_Esq = Esquerda; 
    % a ultima ilha de Esquerda (com a maior coluna) esta mais perto da Ilha e eh sua vizinha 
    (nth1(LenE, Esquerda, Viz_Esq),   
    Lst_Viz_Esq = [Viz_Esq])),
    findall(I, (member(I, Mesma_Linha), I =..[ilha, _, (_,Col)], Col > C), Direita),    % lista de ilhas na mesma linha ah direita 
    % nao havendo ilhas ah direita, a vizinha da esquerda eh a unica na mesma linha
    (Direita = [] -> Vizinhas_Linha = Lst_Viz_Esq;
    % a primeira vizinha da direita tem menor coluna e esta mais perto da Ilha
    (nth1(1, Direita, Viz_Dir), 
    append(Lst_Viz_Esq, [Viz_Dir], Vizinhas_Linha))))),
    % nao havendo ilhas na mesma coluna, as unicas vizinhas serao as da mesma linha 
    (Mesma_Coluna = [] -> Vizinhas = Vizinhas_Linha; 
    (findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin < L), Acima),   % lista de ilhas na mesma coluna acima 
    length(Acima, LenA),
    % havendo apenas 0 ou 1 ilha acima, sera essa a lista da vizinha acima (ou lista vazia) que se une ahs vizinhas da mesma linha
    (LenA =< 1 -> append(Acima, Vizinhas_Linha, Viz_Sem_Abaixo); 
    nth1(LenA, Acima, Viz_Acima),   
    % a ilha acima com maior linha (a ultima da lista) esta mais perto da Ilha e eh sua vizinha
    append([Viz_Acima], Vizinhas_Linha, Viz_Sem_Abaixo)),
    findall(I, (member(I, Mesma_Coluna), I =..[ilha, _, (Lin,_)], Lin > L), Abaixo),   % lista de ilhas na mesma coluna abaixo
    % nao havendo ilhas abaixo, as vizinhas estao ja definidas em Viz_Sem_Abaixo
    (Abaixo = [] -> Vizinhas = Viz_Sem_Abaixo;
    % a primeira ilha abaixo tem menor linha e logo esta mais perto da Ilha e eh sua vizinha
    (nth1(1, Abaixo, Viz_Abaixo),
    append(Viz_Sem_Abaixo, [Viz_Abaixo], Vizinhas))))). 


% ____________________________________________________________________
% 2.4 
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
% 2.5 
% ____________________________________________________________________

% funcao auxiliar 
posicoes_entre_aux(CL1, CL2, CL_Comum, N, Posicoes) :-
    findall(CL_P, (between(CL1, CL2, CL_P), CL_P \== CL1, CL_P \== CL2), Lista),
    (N == 0 -> findall(P, (P = (CL_Comum, CL_P), member(CL_P, Lista)), Posicoes); % posicoes na mesma linha (N=0), entre as colunas de Pos1 e Pos2
    findall(P, (P = (CL_P, CL_Comum), member(CL_P, Lista)), Posicoes)). % posicoes na mesma coluna, entre as linhas de Pos1 e Pos2


% posicoes_entre(Pos, Pos, []).     % nao sei se deve dar falso ou []

posicoes_entre(Pos1, Pos2, Posicoes) :-
    Pos1 \== Pos2,
    Pos1 = (L1, C1),
    Pos2 = (L2, C2),
    % tem ou linhas ou colunas iguais, mas nao ambas pois posicoes iguais nao teriam nenhumas entre elas
    (L1 == L2; C1 == C2),  
    ( L1 == L2 -> (C1 < C2 -> posicoes_entre_aux(C1, C2, L1, 0, Posicoes);   
    posicoes_entre_aux(C2, C1, L1, 0, Posicoes));    % se C1 > C2
    % se L1 \== L2, entao C1 == C2 necessariamente
    (L1 < L2 -> posicoes_entre_aux(L1, L2, C1, 1, Posicoes);  
    (posicoes_entre_aux(L2, L1, C1, 1, Posicoes)))).    % se L1 > L2

% ____________________________________________________________________
% 2.6
% ____________________________________________________________________

cria_ponte(Pos1, Pos2, Ponte) :-
    Pos1 = (L1, C1),
    Pos2 = (L2, C2),
    ((L1 < L2; C1 < C2) -> Ponte =.. [ponte, Pos1, Pos2];   % ordenar posicoes
    Ponte =.. [ponte, Pos2, Pos1]).

% ____________________________________________________________________
% 2.7 - caminho_livre/5
% diz se Pos1 e Pos2 continuam a ser vizinhas apos adicionar ponte(Pos1, Pos2)
% ____________________________________________________________________


caminho_livre(_, _, Posicoes, I, Vz) :-
    I =.. [ilha, _, Pos_I],
    Vz =.. [ilha, _, Pos_Vz],
    posicoes_entre(Pos_I, Pos_Vz, Pos_entre_Vizinhas),
    (Pos_entre_Vizinhas == Posicoes; caminho_livre_aux(Pos_entre_Vizinhas, Posicoes)).  % se o caminho entre as ilhas vizinhas for igual ao da ponte, entao a ponte eh entre elas

caminho_livre_aux([], _). %:- true
caminho_livre_aux([P|R], Posicoes) :-
    exclude(==(P), Posicoes, Posicoes2),
    Posicoes2 == Posicoes,  % se Posicoes nao se alterou, entao nao coincidia com a posicao P 
    caminho_livre_aux(R, Posicoes).

% ____________________________________________________________________
% 2.8 - actualiza_vizinhas_entrada/5
% 
% ____________________________________________________________________

% adicionar ponte(Pos1, Pos2) 
% Posicoes = lista ordenada de pos entre Pos1 e Pos2
% Nova_Entrada == Entrada, excepto na lista de ilhas vizinhas -> remover as ilhas que deixaram de ser vizinhas, após a adição da ponte.
% entrada = [ilha, Vizinhas, Lista de pontes]

actualiza_vizinhas_entrada(_, _, Posicoes, Entrada, Nova_Entrada) :-
    Entrada = [_, Vizinhas, _], !,   % por no actualiza_aux?
    % append(Lista_Pontes, [cria_ponte(Pos1, Pos2, Ponte)], Nova_Lista_Pontes),   % adicona ponte(Pos1, Pos2)
    actualiza_vizinhas_entrada_aux(_, _, Posicoes, Entrada, Vizinhas, Vizinhas, Nova_Entrada).

actualiza_vizinhas_entrada_aux(_, _, _, Entrada, _, [], Entrada).
actualiza_vizinhas_entrada_aux(_, _, Posicoes, Entrada, Vizinhas, [P|R], Nova_Entrada) :- 
    (caminho_livre(_, _, Posicoes, Ilha, P) -> actualiza_vizinhas_entrada_aux(_, _, Posicoes, Entrada, Vizinhas, R, Nova_Entrada); 
    exclude(==(P), Vizinhas, Vizinhas_Atualizadas), Nova_Entrada = [Ilha, Vizinhas_Atualizadas, []]).   % con vazio???
    

% actualiza_vizinhas_entrada((4,1), (4,5), [(4, 2), (4, 3), (4, 4)], [ilha(2, (1,3)), [ilha(2, (3,3))], []], NovaEntrada)

