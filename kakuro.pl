% Diogo Gaspar, 99207, LEIC-A
% Projeto de LP, 2020/2021 - kakuro

:- [codigo_comum].

% -------------------------------ESTRUTURA-espaco---------------------------- %

% cria_espaco(Soma, Vars, Espaco) - construtor
% cria um novo espaco dadas uma soma e um espaco (lista de variaveis)

cria_espaco(Soma, Vars, espaco(Soma, Vars)).

% soma_espaco(Espaco, Soma) - seletor
% devolve a soma do espaco dado como argumento

soma_espaco(espaco(Soma, _), Soma).

% vars_espaco(Espaco, Vars) - seletor
% devolve o espaco (lista de variaveis) do espaco dado como argumento

vars_espaco(espaco(_, Vars), Vars).

% ----------------------------PREDICADOS PRINCIPAIS-------------------------- %

% combinacoes_soma(N, Els, Soma, Combs)
% Combs e a lista ordenada de combinacoes N a N dos elementos de Els 
% com soma Soma

combinacoes_soma(N, Els, Soma, Combs) :-
  findall(Comb, (combinacao(N, Els, Comb), sumlist(Comb, Soma)), Combs).

%-----------------------------------------------------------------------------%

% permutacoes_soma(N, Els, Soma, Perms)
% Perms e a lista ordenada cujos elementos sao as permutacoes 
% das combinacoes N a N dos elementos Els com soma Soma

%-----------------------------------------------------------------------------%

permutacoes_soma(N, Els, Soma, Perms) :-
  combinacoes_soma(N, Els, Soma, Combs),
  findall(Perm, (member(Comb, Combs), permutation(Comb, Perm)), AuxPerms),
  sort(AuxPerms, Perms).

%-----------------------------------------------------------------------------%

% espaco_fila(Fila, Esp, H_V)
% Esp e um espaco de Fila, considernando a "soma vertical/horizontal"
% consoante H_V seja v ou h, respetivamente

espaco_fila(Fila, Esp, H_V) :-
	append([_, [ListaSomas|VarsEsp], Fim], Fila),
	is_list(ListaSomas),
	fim_possivel(Fim),
	VarsEsp \== [], % o forall abaixo vai dar true se VarsEsp == []
	forall(member(Membro, VarsEsp), var(Membro)),
	acessa_indice(ListaSomas, H_V, SomaEsp),
	cria_espaco(SomaEsp, VarsEsp, Esp).

% acessa_indice(Lista, H_V, Soma)
% Soma e a soma a retirar do indice 0 ou 1 de Lista, consoante H_V seja v ou h

acessa_indice([Soma, _], v, Soma) :- !.

acessa_indice([_, Soma], h, Soma).

% fim_possivel(Fim)
% verifica se Fim e um fim possivel para fazer um espaco - verifica se a fila
% acaba ou se ainda ha mais uma lista a frente

fim_possivel([]) :- !.

fim_possivel([P|_]) :- is_list(P).

%-----------------------------------------------------------------------------%

% espacos_fila(H_V, Fila, Espacos)
% predicado que reune em Espacos os espacos de Fila, com soma decidida por H_V

espacos_fila(H_V, Fila, Espacos) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos), !.

espacos_fila(_, _, []).

%-----------------------------------------------------------------------------%

% espacos_puzzle(Puzzle, Espacos)
% Espacos e a lista de espacos de Puzzle

espacos_puzzle(Puzzle, Espacos) :-
	mat_transposta(Puzzle, Transp),
	maplist(espacos_fila(h), Puzzle, Horizontal),
	maplist(espacos_fila(v), Transp, Vertical),
	append(Horizontal, Vertical, PorAlisar),
	append(PorAlisar, Espacos).

%-----------------------------------------------------------------------------%

% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% Esps_com reune todos os espacos de Espacos que tenham uma variavel em comum
% com Esp

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
  vars_espaco(Esp, VarsEsp),
	include(algum(VarsEsp), Espacos, Esps_com).

% algum(Vars, Espaco)
% verifica se Vars tem variaveis em comum com Espaco

algum(Vars, Espaco) :-
	vars_espaco(Espaco, VarsEsp),
	Vars \== VarsEsp, % para nao incluir o proprio espaco
	bagof(Var, (member(Var, Vars), pertence(VarsEsp, Var)), _).

% pertence(Lista, Membro)
% verifica se Membro pertence a Lista, sem unificar

pertence([P|_], X) :- X == P, !.

pertence([_|R], X) :- pertence(R, X).

%-----------------------------------------------------------------------------%

% permutacoes_soma_espacos(Espacos, Perms_soma)
% Perms_soma e uma lista de listas de 2 elementos, onde o primeiro elemento
% e um espaco de Espacos e o segundo a lista ordenada de permutacoes com soma
% igual a soma do espaco

permutacoes_soma_espacos([], []) :- !.

permutacoes_soma_espacos([Esp|R], [[Esp, Perms]|Perms_soma]) :-
	soma_espaco(Esp, SomaEsp),
	faz_lista(SomaEsp, Possiveis),
	vars_espaco(Esp, VarsEsp),
	length(VarsEsp, CompEsp),
	permutacoes_soma(CompEsp, Possiveis, SomaEsp, Perms),  
	permutacoes_soma_espacos(R, Perms_soma).

% faz_lista(N, L)
% predicado auxiliar, faz uma lista de elementos 1 ate N, caso 1 <= N < 9; 
% de 1 a 9 caso N >= 9

faz_lista(N, L) :-
	N < 9 -> numlist(1, N, L);
	numlist(1, 9, L).

%-----------------------------------------------------------------------------%

% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% Perm e uma permutacao possivel para o espaco Esp, dados os espacos Espacos
% onde Esp esta "inserido" e Perms_soma uma lista tal como obtida no predicado
% permutacoes_soma_espacos

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
	include(condensa(Comuns), Perms_soma, PermsComuns),
	permutacoes_soma_espacos([Esp], [[Esp, PermsEsp]]),
	vars_espaco(Esp, VarsEsp),
	member(Perm, PermsEsp),
	unificaveis(Perm, VarsEsp, PermsComuns).

% condensa(Espacos, Perm_soma)
% predicado auxiliar que verifica se o espaco de uma lista igual as sublistas
% da lista obtida em permutacoes_soma_espacos (Perm_soma) pertence a Espacos
condensa(Esps, [Esp, _]) :- pertence(Esps, Esp).

% unificaveis(Perm, Vars, PermsComuns)
% predicado que verifica se, "substituindo" uma variavel de Vars pelo valor
% no sitio correspondente em Perm, continua a haver pelo menos uma
% permutacao no espaco em comum com essa variavel que pode unificar

unificaveis([Valor|R], [Var|Resto_vars], [[Esp, Perms]|Resto_perms]) :-
	vars_espaco(Esp, VarsEsp),
	substitui(Valor, Var, VarsEsp, VarsSubst),
	\+ forall(member(Perm, Perms), \+ =(Perm, VarsSubst)), !,
	unificaveis(R, Resto_vars, Resto_perms).

unificaveis(_, _, []).

% substitui(Valor, Var, Vars, VarsSubst)
% VarsSubst corresponde a substituicao de Var, pertencente a Vars, por Valor

substitui(Valor, Var, [VarDif|R], [VarDif|T]) :-
	Var \== VarDif, !,
	substitui(Valor, Var, R, T).

substitui(Valor, _, [_|R], [Valor|T]) :- substitui(_, _, R, T).

substitui(_, _, [], []).

%-----------------------------------------------------------------------------%

% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% Perms_poss e uma lista de 2 elementos, o primeiro sendo a lista de variaveis
% de Esp e o segundo a lista de permutacoes possiveis para Esp, consoante
% os seus espacos Espacos e respetivas Perms_soma (obtidas tal como no
% predicado permutacoes_soma_espacos)

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [VarsEsp, Perms]) :-
	vars_espaco(Esp, VarsEsp),
	bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms).

%-----------------------------------------------------------------------------%

% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Perms_poss_esps reune todas as permutacoes possiveis para todos os espacos
% Espacos, conforme descrito na seccao 2.1, passo 2 do enunciado

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
	permutacoes_soma_espacos(Espacos, Perms_soma),
	maplist(permutacoes_possiveis_espaco(Espacos, Perms_soma), Espacos, Perms_poss_esps).

%-----------------------------------------------------------------------------%

% numeros_comuns(Lst_Perms, Numeros_comuns)
% Numeros_comuns e uma lista de pares (Pos, Num), onde cada par significa que
% em toda a posicao Pos de Lst_Perms, Num esta presente

numeros_comuns(Lst_Perms, Numeros_comuns) :-
	nth0(0, Lst_Perms, SubLista),
	length(SubLista, Comp),
	findall((Indice, El), (between(1, Comp, Indice), nth1(Indice, SubLista, El),
					maplist(iguais(Indice, El), Lst_Perms)), Numeros_comuns).

% iguais(Indice, El, Perm)
% predicado auxiliar que verifica se El esta presente em Perm no indice Indice
iguais(Indice, El, Perm) :- nth1(Indice, Perm, El).

%-----------------------------------------------------------------------------%

% atribui_comuns(Perms_possiveis)
% atualiza uma lista de permutacoes possiveis, Perms_possiveis, atribuindo a
% cada espaco numeros comuns a todas as permutacoes possiveis para esse espaco

atribui_comuns([[Vars, Perms]|R]) :-
	!,
	numeros_comuns(Perms, Comuns),
	substitui_vars(Vars, Comuns),
	atribui_comuns(R).

atribui_comuns([]).

% substitui_vars(Vars, Numeros_comuns)
% predicado auxiliar que, para todos os pares comuns, substitui a variavel de
% Vars de um dado Indice pelo Valor respetivo

substitui_vars(Vars, [(Indice, Valor)|R]) :-
	nth1(Indice, Vars, Valor), !,
	substitui_vars(Vars, R).

substitui_vars(_, []).

%-----------------------------------------------------------------------------%

% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis e o resultado de retirar todas as permutacoes
% impossiveis a Perms_Possiveis

retira_impossiveis([[Vars, Perms]|R], [[Vars, NovasPossiveis]|T]) :-
	!,
	bagof(Perm, (member(Perm, Perms), \+ \+ =(Vars, Perm)), NovasPossiveis),
	retira_impossiveis(R, T).

retira_impossiveis([], []).

%-----------------------------------------------------------------------------%

% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis e o resultado de simplificar Perms_Possiveis, tal como
% descrito na seccao 2.1, passo 3 do enunciado

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Temp),
	(Temp == Perms_Possiveis -> Novas_Perms_Possiveis = Temp;
	simplifica(Temp, Novas_Perms_Possiveis)).

%-----------------------------------------------------------------------------%

% inicializa(Puzzle, Perms_Possiveis)
% Perms_Possiveis e a lista de permutacoes possiveis simplificada para Puzzle

inicializa(Puzzle, Perms_Possiveis) :-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
	simplifica(Perms_poss_esps, Perms_Possiveis).

%-----------------------------------------------------------------------------%

% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Escolha e o elemento de uma lista de permutacoes possiveis, Perms_Possiveis,
% escolhido segundo o criterio indicado na seccao 2.2 do enunciado

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
	exclude(perm_unitaria, Perms_Possiveis, Nao_Unitarias),
	Nao_Unitarias \== [],
	menor_perm(Nao_Unitarias, Escolha).

% perm_unitaria(Lst_Perms)
% verifica se Lst_Perms tem apenas uma permutacao

perm_unitaria([_, Perms]) :- 
	length(Perms, Comp), Comp == 1.

% menor_perm(Perms, Escolha)
% Escolha corresponde ao elemento de Perms escolhido segundo o criterio
% indicado na seccao 2.2 do enunciado

menor_perm([[Vars, Perms]|R], Escolha) :-
	length(Perms, Comp),
	menor_perm(R, [Vars, Perms], Escolha, Comp).

menor_perm([], Escolha, Escolha, _) :- !.

menor_perm([[Vars, Perms]|R], Atual, Escolha, CompAtual) :-
	length(Perms, Comp),
	Comp < CompAtual -> menor_perm(R, [Vars, Perms], Escolha, Comp);
	menor_perm(R, Atual, Escolha, CompAtual).

%-----------------------------------------------------------------------------%

% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis e uma lista de permutacoes possiveis e Escolha um dos seus
% elementos, escolhido pelo predicado escolhe_menos_alternativas;
% escolhe uma permutacao de Lst_Perms, Perm;
% unifica Esp com Perm;
% Novas_Perms_Possiveis e o resultado de substituir em Perms_Possiveis
% a Escolha por [Esp, [Perm]]

experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis) :-
	member(Perm, Lst_Perms),
	Esp = Perm,
	append(Inicio, [[Esp, Lst_Perms]|Fim], Perms_Possiveis),
	append(Inicio, [[Esp, [Perm]]|Fim], Novas_Perms_Possiveis).

%-----------------------------------------------------------------------------%

% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Novas_Perms_Possiveis e o resultado de aplicar o algoritmo descrito na seccao
% 2.2 do enunciado a Perms_Possiveis, uma lista de permutacoes possiveis

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
	escolhe_menos_alternativas(Perms_Possiveis, Escolha), !,
	experimenta_perm(Escolha, Perms_Possiveis, AposExp),
	simplifica(AposExp, AposSimp),
	resolve_aux(AposSimp, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
	simplifica(Perms_Possiveis, Novas_Perms_Possiveis).

%-----------------------------------------------------------------------------%

% resolve(Puz)
% resolve o puzzle Puz, ou seja, apos a invocacao deste predicado a grelha de
% Puz tem todas as variaveis substituidas por numeros que respeitam as suas
% restricoes

resolve(Puzzle) :-
	inicializa(Puzzle, Perms_Possiveis),
	resolve_aux(Perms_Possiveis, _).