 /* Nome: Catarina Soares da Costa Bento
    Numero: 93230 */

:-[codigo_comum].

%-------------------------------------------------------------------------------
% obtem_letras_palavras(Lst_Pals, Letras), em que Lst_Pals e uma lista de 
% palavras, significa que Letras e a lista ordenada cujos elementos sao listas 
% com as letras de cada palavra de Lst_Pals.
%-------------------------------------------------------------------------------

obtem_letras_palavras(Lst_Pals, Letras) :-
    msort(Lst_Pals, Lst_Pals_Sorted),
    maplist(atom_chars, Lst_Pals_Sorted, Letras).

   
%-------------------------------------------------------------------------------
% espaco_fila(Fila, Esp), em que Fila e uma fila (linha ou coluna) de uma
% grelha, significa que Esp e um espaco de Fila.
%-------------------------------------------------------------------------------
espaco_fila(Fila, Esp) :-
    espaco_fila(Fila, [] , Esp).

espaco_fila([], Aco, Aco) :-
    length(Aco, N),
    N > 2,
    !.

espaco_fila([P | _], Aco, Aco) :-
    P == #,
    length(Aco, N),
    N > 2.

espaco_fila([P | R], _, Esp) :-
    P == #,
    !,
    espaco_fila(R, [], Esp).    
    
espaco_fila([P | R], Aco, Esp) :-
    P \== #,
    !,
    append(Aco, [P], N_Aco),
    espaco_fila(R, N_Aco, Esp).


%-------------------------------------------------------------------------------
% espacos_fila(Fila, Espacos), em que Fila e uma fila (linha ou coluna) de uma 
% grelha, significa que Espacos e a lista de todos os espacos de Fila, 
% da esquerda para a direita.
%-------------------------------------------------------------------------------
espacos_fila(Fila, Espacos) :-
    bagof(Espaco, espaco_fila(Fila, Espaco), Espacos),
    !.

espacos_fila(_, []).


%-------------------------------------------------------------------------------
% membro_lista(El, L), significa que El e elemento da lista L.
%-------------------------------------------------------------------------------
membro_lista(El,[P|_]) :-
    El==P.

membro_lista(El,[_|R]) :-
    membro_lista(El,R).

%-------------------------------------------------------------------------------
% espacos_puzzle(Grelha, Espacos), em que Grelha e uma grelha, significa que 
% Espacos e a lista de espacos de Grelha.
%-------------------------------------------------------------------------------
espacos_puzzle(Grelha, Espacos) :-
    espacos_puzzle(Grelha, [], Espacos_Linhas),
    mat_transposta(Grelha, Grelha_Colunas),
    espacos_puzzle(Grelha_Colunas, [], Espacos_Colunas),
    append(Espacos_Linhas, Espacos_Colunas, Espacos).

espacos_puzzle([], Aco, Aco).

espacos_puzzle([P | R], Aco, Espacos) :-
    espacos_fila(P, Esp),
    delete(Esp, [], N_Esp),    
    append(Aco, N_Esp, N_Aco),
    espacos_puzzle(R, N_Aco, Espacos).

%-------------------------------------------------------------------------------
% elemento_comum(L, Esp), significa que L e Esp tem pelo menos um elemento
% em comum.
%-------------------------------------------------------------------------------
elemento_comum([P | _], Esp) :-
    membro_lista(P, Esp),
    !.

elemento_comum([_ | R], Esp) :-
    elemento_comum(R, Esp).


%-------------------------------------------------------------------------------
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos e uma 
% lista de espacos e Esp e um espaco, significa que Esps_com e a lista de 
% espacos com variaveis em comum com Esp, exceptuando Esp. Os espacos em
% Esps_com aparecem pela mesma ordem que aparecem em Esps.
%-------------------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com):-
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com, []).

espacos_com_posicoes_comuns([], _, Temp, Temp).

espacos_com_posicoes_comuns([P | R], Esp, Esps_com, Temp):-
    elemento_comum(P, Esp),
    P \== Esp,
    append(Temp, [P], N_Temp),
    !,
    espacos_com_posicoes_comuns(R, Esp, Esps_com, N_Temp).

espacos_com_posicoes_comuns([_ | R], Esp, Esps_com, Temp):-
    espacos_com_posicoes_comuns(R, Esp, Esps_com, Temp).

%-------------------------------------------------------------------------------
% verificar_letras_iguais(Pal, Esp), em que Esp e um espaco, significa que
% se Esp tiver um elemento nao variavel, ou seja uma letra, num certo indice,
% a palavra Pal tambem tem essa mesma letra nessa posicao.
%-------------------------------------------------------------------------------
verificar_letras_iguais([], []).

verificar_letras_iguais([P1 | R1], [P2 | R2]) :-
    nonvar(P2),
    !,
    P1 == P2,
    verificar_letras_iguais(R1, R2).

verificar_letras_iguais([_ | R1], [_ | R2]) :-
    verificar_letras_iguais(R1, R2).

%-------------------------------------------------------------------------------
% indice_em_comum(Esp1, Esp2, Indice), em que Esp1 e Esp2 sao espacos que tem
% uma variavel em comum, significa que Indice e o indice de Esp1 em que se 
% os espacos se intersetam (com o indice das listas a comecar em zero).
%-------------------------------------------------------------------------------
indice_em_comum(Esp1, Esp2, Indice) :-
    indice_em_comum(Esp1, Esp2, Indice, 0).

indice_em_comum([P | _], Esp, Temp, Temp) :-
    membro_lista(P,Esp),
    !.

indice_em_comum([_ | R], Esp, Indice, Temp) :-
    N_Temp is Temp + 1,
    indice_em_comum(R, Esp, Indice, N_Temp). 


%-------------------------------------------------------------------------------
% verificar_unificacao_espacos(Esp_Coms, Esp, Pal, Letras), em que Esp e um
% espaco, Esp_Coms e uma lista dos espacos em Comum com Esp, Pal e uma palavra e 
% Letras e uma lista de listas das letras de cada palavra, significa que e 
% possivel unificar Pal com Esp, havendo pelo menos uma Palavra em Letras
% que consiga unificar com cada elemento de Esp_Coms nesse caso.
%-------------------------------------------------------------------------------

verificar_unificacao_espacos([], _, _, _).

verificar_unificacao_espacos([P | R], Esp, Pal, Letras) :- 
 verificar_unificacao_espaco(P, Esp, Pal, Letras), 
 !,
 verificar_unificacao_espacos(R, Esp, Pal, Letras).

%-------------------------------------------------------------------------------
% verificar_unificacao_espaco(Esp_Com, Esp, Pal, Letras), em que Esp e um
% espaco, Esp_Com e um espaco em Comum com Esp, Pal e uma palavra e Letras e uma
% lista de listas das letras de cada palavra, significa que e possivel unificar 
% Pal com Esp, havendo pelo menos uma Palavra em Letras que consiga unificar com 
% Esp_Coms nesse caso.
%-------------------------------------------------------------------------------
verificar_unificacao_espaco(P, Esp, Pal, Letras) :-
    indice_em_comum(P, Esp, Indice_Primeiro),
    indice_em_comum(Esp, P, Indice_Segundo),
    nth0(Indice_Segundo, Pal, El),
    verificar_letra_indice(Indice_Primeiro, Letras, El, P).


%-------------------------------------------------------------------------------
% verificar_letra_indice(Indice, Letras, El, Esp), em que Esp e um espaco em 
% comum com outro Espaco1, Letras e uma lista de listas das letras de cada 
% palavra, Indice e o indice de Esp em que Esp e Espaco1 se intersetam e El e 
% o elemento de Esp nesse indice, significa que existe pelo menos uma palavra
% de Letras que tem El no seu indice Indice e cumpre as regras de unificacao 
% com Esp.
%-------------------------------------------------------------------------------
verificar_letra_indice(Indice, [P | _], El, Esp) :-
    nth0(Indice, P, Elemento),
    Elemento == El,
    verificar_letras_iguais(P, Esp),
    !.

verificar_letra_indice(Indice, [_| R], El, Esp) :-
    verificar_letra_indice(Indice, R , El, Esp).


%-------------------------------------------------------------------------------
% palavra_possivel_esp(Pal, Esp, Espacos, Letras), em que Pal e uma lista de 
% letras de uma palavra, Esp e um espaco, Espacos e uma lista de espacos, e 
% Letras e uma lista de listas de letras de palavras, significa que Pal e uma 
% palavra possivel para o espaco Esp.
%-------------------------------------------------------------------------------
palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    !,
    verificar_letras_iguais(Pal, Esp),
    espacos_com_posicoes_comuns(Espacos, Esp, Esp_Com),
    verificar_unificacao_espacos(Esp_Com, Esp, Pal, Letras).

%-------------------------------------------------------------------------------
% palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis), em que Letras e 
% uma lista de listas de letras de palavras, Espacos e uma lista de espacos, 
% Esp e um espaco, significa que Pals_Possiveis e a lista ordenada de palavras
% possiveis para o espaco Esp.
%-------------------------------------------------------------------------------
palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    palavras_possiveis_esp(Letras, Letras, Espacos, Esp, Pals_Possiveis, []).

palavras_possiveis_esp([], _, _, _, Temp, Temp).

palavras_possiveis_esp([P | R], Letras, Espacos, Esp, Pals_Possiveis, Temp) :-
    palavra_possivel_esp(P, Esp, Espacos, Letras),
    append(Temp, [P],N_Temp),
    !,
    palavras_possiveis_esp(R, Letras, Espacos, Esp, Pals_Possiveis, N_Temp).

palavras_possiveis_esp([_ | R], Letras, Espacos, Esp, Pals_Possiveis, Temp) :-
    !,
    palavras_possiveis_esp(R, Letras, Espacos, Esp, Pals_Possiveis, Temp).

%-------------------------------------------------------------------------------
% palavras_possiveis(Letras, Espacos, Pals_Possiveis), em que Letras e uma lista
% de listas de letras de palavras e Espacos e uma lista de espacos, significa 
% que Pals_Possiveis e a lista de palavras possiveis.
%-------------------------------------------------------------------------------
palavras_possiveis(Letras, Espacos, Pals_Possiveis) :-
    palavras_possiveis(Letras, Espacos, Espacos, Pals_Possiveis, []).

palavras_possiveis(_, [], _, Temp, Temp).

palavras_possiveis(Letras, [P | R], Espacos, Pals_Possiveis, Temp) :-
    palavras_possiveis_esp(Letras, Espacos, P, Pals),
    append(Temp, [[P,Pals]], N_Temp),
    palavras_possiveis(Letras, R, Espacos, Pals_Possiveis, N_Temp),
    !.

%-------------------------------------------------------------------------------
% letras_comuns(Lst_Pals, Letras_comuns), em que Lst_Pals e uma lista de listas 
% de letras, significa que Letras_comuns e uma lista de pares(Pos, Letra), 
% significando que as listas de Lst_Pals contem a letra Letra na posicao Pos.
%-------------------------------------------------------------------------------
letras_comuns([], []):-
    !.

letras_comuns(Lst_Pals, Letras_comuns) :-
    letras_comuns(Lst_Pals, Letras_comuns, 1, []),
    !.

letras_comuns(Lst_Pals, Letras_comuns, Indice, Letras_comuns) :-
    nth1(1, Lst_Pals, Elemento),
    length(Elemento, N),
    Indice =:= N + 1,
    !.

letras_comuns([P|R], Letras_comuns, Indice, Temp) :-
    nth1(Indice, P, Elemento),
    letra_comum(R, Temporario, Indice, Elemento),
    length(Temporario, N),
    N > 0,
    append(Temp, Temporario, N_Temporario),
    Indice_Mais_Um is Indice +1,
    !,
    letras_comuns([P | R], Letras_comuns, Indice_Mais_Um, N_Temporario).

letras_comuns(Lst, Letras_comuns, Indice, Temp) :-
    Indice_Mais_Um is Indice +1,
    letras_comuns(Lst, Letras_comuns, Indice_Mais_Um, Temp).

%-------------------------------------------------------------------------------
% letra_comum(Palavras, Letras_comuns, Indice, Elemento), em que Palavras e uma 
% lista de letras de palavras que tem o mesmo elemento Elemento no indice Indice 
% significa que Letras_comuns e uma lista da forma [(Indice, Elemento)].
%-------------------------------------------------------------------------------
letra_comum([], Letras_comuns, Indice, Elemento) :-
    append([], [(Indice, Elemento)], Letras_comuns).

letra_comum([P|R], Letras_comuns, Indice, Elemento) :-
    nth1(Indice, P, El),
    El == Elemento,
    !,
    letra_comum(R, Letras_comuns, Indice, Elemento).

%-------------------------------------------------------------------------------
% atribui_comuns(Pals_Possiveis), em que Pals_Possiveis e uma lista de palavras 
% de letras, significa que Letras_comuns e uma lista de pares(Pos, Letra), 
% possiveis, actualiza esta lista atribuindo a cada espaco as letras comuns a 
% todas as palavras possiveis para esse espaco.
%-------------------------------------------------------------------------------
atribui_comuns([]).

atribui_comuns([P | R]) :-
    atribui_comuns_espaco(P),
    atribui_comuns(R),
    !.

%-------------------------------------------------------------------------------
% atribui_comuns_espaco(Lista), em que o primeiro elemento de Lista e um Espaco
% e o segundo elemento sao as suas palavras possiveis, e encontrado a Lista 
% Letras_comuns que e lista de pares(Pos, Letra), tal que essa Letra e unificada
% com a variavel na posicao Pos do Espaco.
%-------------------------------------------------------------------------------

atribui_comuns_espaco([P | [P1]]) :-
    letras_comuns(P1, Lst_Comuns),
    unifica_comuns(P, Lst_Comuns).

%-------------------------------------------------------------------------------
% unifica_comuns(Espaco, Lst_Comuns), em que cada elemento de Lst_Comuns esta
% no formato (Indice, Elemento), unifica o elemento no indice Indice de Espaco
% com o Elemento para cada elemento da Lista Lst_Comuns.
%-------------------------------------------------------------------------------
unifica_comuns(_, []). 

unifica_comuns(Esp, [ (Indice, Elemento) | R]) :-
    nth1(Indice, Esp, Elemento),
    !,
    unifica_comuns(Esp, R).

%-------------------------------------------------------------------------------
% retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis),em que Pals_Possiveis
% e uma lista de palavras posseveis, significa que Novas_Pals_Possiveis e o
% resultado de tirar palavras impossiveis de Pals_Possiveis. 
%-------------------------------------------------------------------------------
retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis) :-
    retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis, []).

retira_impossiveis([], Pals_Possiveis, Pals_Possiveis).

retira_impossiveis([P | R], Pals_Possiveis, Temp):-
    retira_impossiveis_esp(P, Novo_espaco),
    append(Temp, [Novo_espaco], N_Temp),
    retira_impossiveis(R, Pals_Possiveis, N_Temp).

%-------------------------------------------------------------------------------
% retira_impossiveis_esp(Espaco, Novo_espaco), em que Espaco e um Espaco e as suas
% palavras possiveis iniciais, significa que Novo_espaco e essa mesma Lista sem as 
% palavras impossiveis desse espaco.
%-------------------------------------------------------------------------------
retira_impossiveis_esp([P | [R]] ,Novo_espaco):-
    verificar_pal_espaco(P, R, N_Espaco, []),
    append([P], N_Espaco, Novo_espaco).

%-------------------------------------------------------------------------------
% verifica_pal_espaco(Esp, Pals_Possiveis, Novo_Espaco, Temp), em que Esp e um
% espaco, Pals_Possiveis sao as Palavras Possiveis Iniciais de Esp, e Temp um 
% acumulador temporario, significa que Novo_Espaco e uma lista com as Palavras
% Possiveis (removidas as palavras impossiveis das Palavras Possiveis Iniciais).
%-------------------------------------------------------------------------------
verificar_pal_espaco(_, [], [Novo_espaco], Novo_espaco).

verificar_pal_espaco(Esp, [P | R], Novo_espaco, Temp) :-
    verificar_letras_iguais(P, Esp),
    append(Temp, [P], N_Temp),
    verificar_pal_espaco(Esp, R, Novo_espaco, N_Temp),
    !.

verificar_pal_espaco(Esp, [_ | R], Novo_espaco, Temp) :-
    verificar_pal_espaco(Esp, R, Novo_espaco, Temp).

%-------------------------------------------------------------------------------
% obtem_unicas(Pals_Possiveis, Unicas), em que Pals_Possiveis e uma lista de  
% palavras possiveis, significa que Unicas e a lista de palavrasu unicas de 
% Pals_Possiveis. 
%-------------------------------------------------------------------------------
obtem_unicas(Pals_Possiveis, Unicas) :-
    obtem_unicas(Pals_Possiveis, Unicas, []).

obtem_unicas([], Unicas, Unicas).

obtem_unicas([[_ | [R1]] | R], Unicas, Temp) :-
    length(R1, N),
    N == 1,
    append(Temp, R1, N_Temp),
    obtem_unicas(R, Unicas, N_Temp),
    !.

obtem_unicas([[_ | [_]] | R], Unicas, Temp) :-
    obtem_unicas(R, Unicas, Temp).

%-------------------------------------------------------------------------------
% retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis), em que Pals_Possiveis e 
% uma lista de palavras possiveis, significa que Novas_Pals_Possiveis e o 
% resultado de retirar de Pals_Possiveis as palavras unicas. 
%-------------------------------------------------------------------------------
retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis) :-
    obtem_unicas(Pals_Possiveis, Unicas),
    retira_unicas(Pals_Possiveis, Unicas, Novas_Pals_Possiveis, []).

retira_unicas([], _, Novas_Pals_Possiveis, Novas_Pals_Possiveis).

retira_unicas([[P | [R1]] | R], Unicas, Novas_Pals_Possiveis, Temp) :-
    length(R1, N),
    N == 1,
    !,
    append([P], [R1], N_Temporario),
    append(Temp, [N_Temporario], N_Temp),
    retira_unicas(R, Unicas, Novas_Pals_Possiveis, N_Temp).

retira_unicas([[P | [R1]] | R], Unicas, Novas_Pals_Possiveis, Temp) :-
    subtract(R1, Unicas, Novas_Palavras),
    append([P], [Novas_Palavras], N_Temporario),
    append(Temp, [N_Temporario], N_Temp),
    retira_unicas(R, Unicas, Novas_Pals_Possiveis, N_Temp).
    
%-------------------------------------------------------------------------------
% simplifica(Pals_Possiveis, Novas_Pals_Possiveis), em que Pals_Possiveis e uma
% lista de palavras possiveis, significa que Novas_Pals_Possiveis e o resultado
% de simplificar Pals_Possiveis. 
%-------------------------------------------------------------------------------
simplifica(Pals_Possiveis, Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, Pals_Possiveis),
    retira_unicas(Pals_Possiveis, Pals_Possiveis),
    !.

simplifica(Pals_Possiveis, Novas_Pals_Possiveis) :-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis, N_Pals_Possiveis),
    retira_unicas(N_Pals_Possiveis, N_P_P_Unicas),
    simplifica(N_P_P_Unicas, Novas_Pals_Possiveis).

%-------------------------------------------------------------------------------
% inicializa(Puz, Pals_Possiveis), em que Puz e um puzzle, significa que 
% Pals_Possiveis e a lista de palavras possiveis simplificada para Puz. 
%-------------------------------------------------------------------------------
inicializa([P | [R]], Novas_Pals_Possiveis) :-
    obtem_letras_palavras(P, Letras),
    espacos_puzzle(R, Espacos),
    palavras_possiveis(Letras, Espacos, Pals_Possiveis),
    simplifica(Pals_Possiveis, Novas_Pals_Possiveis).

%-------------------------------------------------------------------------------
% comprimento_um(Lst), em que Lst e uma lista, significa que a lista Lst tem 
% apenas um elemento.
%-------------------------------------------------------------------------------
comprimento_um(Lst) :-
    length(Lst, N),
    N =:= 1.

%-------------------------------------------------------------------------------
% menor_comprimento(Lista, Menor_comprimento, acumulador), em que Lista e uma 
% lista de listas de Palavras Possiveis de certos espacos, Menor_comprimento e 
% o comprimento da menor lista de Palavras Possiveis.
%-------------------------------------------------------------------------------
menor_comprimento([], Menor_comprimento, Menor_comprimento).

menor_comprimento([P | R], Menor_comprimento, 0) :-
    length(P, N),
    !,
    menor_comprimento(R, Menor_comprimento, N).

menor_comprimento([P | R], Menor_comprimento, Temp):-
    length(P, N),
    N < Temp,
    !,
    menor_comprimento(R, Menor_comprimento, N).

menor_comprimento([_ | R], Menor_comprimento, Temp):-
    menor_comprimento(R, Menor_comprimento, Temp).

%-------------------------------------------------------------------------------
% escolhe_menos_alternativas(Pals_Possiveis, Escolha), em que Pals_Possiveis e
% uma lista de palavras possiveis, significa que Escolha e o elemento de 
% Pals_Possiveis escolhido segundo o criterio indicado no enunciado. 
% Se todos os espacos em Pals_Possiveis tiverem associadas listas de palavras 
% unitarias, o predicado deve devolver "falso". 
%-------------------------------------------------------------------------------
escolhe_menos_alternativas(Pals_Possiveis, Escolha) :-
    maplist(nth0(1), Pals_Possiveis, Novas_Pals_Possiveis),
    exclude(comprimento_um, Novas_Pals_Possiveis, N_Pals_Possiveis),
    length(N_Pals_Possiveis, N),
    N > 0,
    menor_comprimento(N_Pals_Possiveis, Menor_comprimento, 0),
    procurar_espaco(Pals_Possiveis, Menor_comprimento, Escolha).

%-------------------------------------------------------------------------------
% procurar_espaco(Lista, Menor_comprimento, P), em que Lista e uma lista de 
% palavras possiveis, P e o primeiro elemento de Lista que tem o comprimento
% Menor_Comprimento.
%-------------------------------------------------------------------------------
procurar_espaco([P | _], Menor_comprimento, P) :-
    nth0(1, P, Elemento),
    length(Elemento, Tamanho),
    Tamanho == Menor_comprimento,
    !.

procurar_espaco([_ | R], Menor_comprimento, Escolha) :-
    procurar_espaco(R, Menor_comprimento, Escolha).

%-------------------------------------------------------------------------------
% dividir_lista(Lista, Elemento, Acumulador, P, S), significa que a lista Lista
% e dividida em duas listas: a primeira, P, com todos os elementos antes de 
% Elemento, e a segunda, S, com todos os elementos depois de Elemento.
%-------------------------------------------------------------------------------
dividir_lista([P | R], Elemento, Aco, Aco, R) :-
    P == Elemento,
    !.

dividir_lista([P | R], Elemento, Aco, Primeira_parte, Segunda_parte) :-
    append(Aco, [P], N_Aco),
    dividir_lista(R, Elemento, N_Aco, Primeira_parte, Segunda_parte) .

%-------------------------------------------------------------------------------
% A chamada experimenta_pal(Escolha, Pals_Possiveis,Novas_Pals_Possiveis), em
% que Pals_Possiveis e uma lista de palavras possiveis, e Escolha e um dos seus
% elementos (escolhido pelo predicado anterior). 
%-------------------------------------------------------------------------------
experimenta_pal([P | [R]], Pals_Possiveis, Novas_Pals_Possiveis) :-
    member(Pal, R),
    P = Pal,
    dividir_lista(Pals_Possiveis, [P | [R]], [], Primeiro, Segundo),
    append(Primeiro, [[P | [[Pal]]]], N_Lista),
    append(N_Lista, Segundo, Novas_Pals_Possiveis).
    
%-------------------------------------------------------------------------------
% resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis), em que Pals_Possiveis e 
% uma lista de palavras possiveis, significa que Novas_Pals_Possiveis e o 
% resultado de aplicar o algoritmo seguinte a Pals_Possiveis.
%-------------------------------------------------------------------------------
resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis) :-
    escolhe_menos_alternativas(Pals_Possiveis, Escolha),
    !,
    experimenta_pal(Escolha, Pals_Possiveis,N_Pals_Possiveis),
    simplifica(N_Pals_Possiveis, N_Pals_S),
    resolve_aux(N_Pals_S, Novas_Pals_Possiveis).

resolve_aux(P, P).

%-------------------------------------------------------------------------------
% resolve(Puz), em que Puz e um puzzle, resolve esse puzzle, isto e, apos a 
% invocacao deste predicado a grelha de Puz tem todas as variaveis substituidas
% por letras que constituem as palavras da lista de palavras de Puz.
%-------------------------------------------------------------------------------
resolve(Puz) :-
    inicializa(Puz, Pals_Possiveis),
    resolve_aux(Pals_Possiveis, _).