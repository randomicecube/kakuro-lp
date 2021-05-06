% Diogo Gaspar, 99207

:- [codigo_comum].

% --------------------------------------------------------------------------- %
% ----------------------------PREDICADOS PRINCIPAIS-------------------------- %
% --------------------------------------------------------------------------- %

% --------------------combinacoes_soma(N, Els, Soma, Combs)------------------ %

combinacoes_soma(N, Els, Soma, Combs) :-
  findall(Comb, (combinacao(N, Els, Comb), sumlist(Comb, Soma)), Combs).

% --------------------permutacoes_soma(N, Els, Soma, Combs)------------------ %

permutacoes_soma(N, Els, Soma, Perms) :-
  combinacoes_soma(N, Els, Soma, Combs),
  findall(Perm, (member(Comb, Combs), permutation(Comb, Perm)), AuxPerms),
  sort(AuxPerms, Perms).

% -------------------------espaco_fila(Fila, Esp, H_V)----------------------- %

espaco_fila(Fila, Esp, H_V) :-
	% as novas variaveis sao a lista aux. para variaveis e a soma atual
	espaco_fila_aux(Fila, Esp, H_V, [], -1). % -1 enquanto nao houver uma soma

% quando chegamos ao fim da fila
espaco_fila_aux([], Esp, _, VarsAtuais, SomaAtual) :-
	VarsAtuais \== [],
	cria_espaco(SomaAtual, VarsAtuais, Esp).

% quando e uma variavel (parte do espaco, portanto)
espaco_fila_aux([P|R], Esp, H_V, VarsAtuais, SomaAtual) :-
	var(P), !,
	append(VarsAtuais, [P], VarsAtualizadas),
	espaco_fila_aux(R, Esp, H_V, VarsAtualizadas, SomaAtual).

% quando e uma lista e vem depois de um espaco
espaco_fila_aux([P|_], Esp, _, VarsAtuais, SomaAtual) :-
	is_list(P),
	VarsAtuais \== [],
	SomaAtual > -1,
	cria_espaco(SomaAtual, VarsAtuais, Esp).

% quando e o primeiro elemento da fila/uma lista depois de outra lista
espaco_fila_aux([P|R], Esp, H_V, _, _) :-
	acessa_indice(P, H_V, NovaSoma),
	espaco_fila_aux(R, Esp, H_V, [], NovaSoma).

% predicado de decisao sobre qual a soma a escolher
acessa_indice([Soma, _], v, Soma).

acessa_indice([_, Soma], h, Soma).

% ------------------------espacos_fila(Fila, Esp, H_V)----------------------- %

espacos_fila(H_V, Fila, Espacos) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos), !.

espacos_fila(_, _, []).

% -----------------------espacos_puzzle(Fila, Esp, H_V)---------------------- %

espacos_puzzle(Puzzle, Espacos) :-
	mat_transposta(Puzzle, Transp),
	maplist(espacos_fila(h), Puzzle, Horizontal),
	maplist(espacos_fila(v), Transp, Vertical),
	append(Horizontal, Vertical, PorAlisar),
	append(PorAlisar, Espacos).

% ------------espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)------------ %

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
  vars_espaco(Esp, VarsEsp),
	include(algum(VarsEsp), Espacos, Esps_com).

algum(Vars, Espaco) :-
	vars_espaco(Espaco, VarsEsp),
	Vars \== VarsEsp,
	bagof(Var, (member(Var, Vars), pertence(VarsEsp, Var)), _).

% predicado auxiliar, semelhante ao member/2 mas sem a unificacao
pertence([P|_], X) :- X == P, !.

pertence([_|R], X) :- pertence(R, X).

% ---------------permutacoes_soma_espacos(Espacos, Perms_soma)--------------- %

permutacoes_soma_espacos([], []).

permutacoes_soma_espacos([Esp|R], [[Esp, Perms]|Perms_soma]) :-
	soma_espaco(Esp, SomaEsp),
	faz_lista(SomaEsp, Possiveis),
	vars_espaco(Esp, VarsEsp),
	length(VarsEsp, CompEsp),
	permutacoes_soma(CompEsp, Possiveis, SomaEsp, Perms),  
	permutacoes_soma_espacos(R, Perms_soma).

% predicado auxiliar, lista de 1 ate N, caso 1 <= N < 9; de 1 a 9 caso N >= 9
faz_lista(N, L) :-
	N < 9 -> findall(Iterador, between(1, N, Iterador), L);
	findall(Iterador, between(1, 9, Iterador), L).

% ---------permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)-------- %

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	espacos_com_posicoes_comuns(Espacos, Esp, Comuns),
	include(condensa(Comuns), Perms_soma, PermsComuns),
	permutacoes_soma_espacos([Esp], [[Esp, PermsEsp]]),
	vars_espaco(Esp, VarsEsp),
	member(Perm, PermsEsp),
	unificaveis(Perm, VarsEsp, PermsComuns).

% predicado auxiliar para compactar Perms_soma, considerando os espacos comuns
condensa(Esps, [Esp, _]) :- pertence(Esps, Esp).

% predicado auxiliar que verifica se Perm e possivel para o espaco,
% fazendo a substituicao da Var em comum com Esp e verificando se continua a
% haver pelo menos uma unificacao possivel 
unificaveis([Valor|R], [Var|Resto_vars], [[Esp, Perms]|Resto_perms]) :-
	vars_espaco(Esp, VarsEsp),
	substitui(Valor, Var, VarsEsp, VarsSubst),
	findall(Perm, (member(Perm, Perms), =(Perm, VarsSubst)), Unificados),
	Unificados \== [], !,
	unificaveis(R, Resto_vars, Resto_perms).

unificaveis(_, _, []).

% predicado auxiliar que realiza uma "falsa substituicao", sem unificar
substitui(Valor, Var, [VarDif|R], [VarDif|T]) :-
	Var \== VarDif, !,
	substitui(Valor, Var, R, T).

substitui(Valor, _, [_|R], [Valor|T]) :- substitui(_, _, R, T).

substitui(_, _, [], []).

% -----permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)---- %

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [VarsEsp, Perms]) :-
	vars_espaco(Esp, VarsEsp),
	bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms).

% -----------permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)--------- %

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
	permutacoes_soma_espacos(Espacos, Perms_soma),
	maplist(permutacoes_possiveis_espaco(Espacos, Perms_soma), Espacos, Perms_poss_esps).

% -----------------numeros_comuns(Lst_Perms, Numeros_comuns)----------------- %

numeros_comuns(Lst_Perms, Numeros_comuns) :-
	nth0(0, Lst_Perms, SubLista),
	length(SubLista, Comp),
	findall((Indice, El), 
		(between(1, Comp, Indice), 
		nth1(Indice, SubLista, El),
		maplist(iguais(Indice, El), Lst_Perms)),
		Numeros_comuns).

% predicado auxiliar que verifica se El esta presente em Perm no indice Indice
iguais(Indice, El, Perm) :- nth1(Indice, Perm, El).

% ----------------------atribui_comuns(Perms_possiveis)---------------------- %

atribui_comuns([[Vars, Perms]|R]) :-
	!,
	numeros_comuns(Perms, Comuns),
	substitui_vars(Vars, Comuns),
	atribui_comuns(R).

atribui_comuns([]).

% predicado auxiliar que, para todos os pares comuns, substitui Var por Valor
% no Indice correto
substitui_vars(Vars, [(Indice, Valor)|R]) :-
	nth1(Indice, Vars, Valor), !,
	substitui_vars(Vars, R).

substitui_vars(_, []).

% ---------retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)-------- %

retira_impossiveis([[Vars, Perms]|R], [[Vars, NovasPossiveis]|T]) :-
	!,
	bagof(Perm, (member(Perm, Perms), \+ \+ =(Vars, Perm)), NovasPossiveis),
	retira_impossiveis(R, T).

retira_impossiveis([], []).

% -------------simplifica(Perms_Possiveis, Novas_Perms_Possiveis)------------ %

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Temp),
	Temp == Perms_Possiveis, !,
	Novas_Perms_Possiveis = Temp.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
	atribui_comuns(Perms_Possiveis),
	retira_impossiveis(Perms_Possiveis, Temp),
	simplifica(Temp, Novas_Perms_Possiveis).

% --------------------inicializa(Puzzle, Perms_Possiveis)-------------------- %

inicializa(Puzzle, Perms_Possiveis) :-
	espacos_puzzle(Puzzle, Espacos),
	permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
	simplifica(Perms_poss_esps, Perms_Possiveis).

% ------------escolhe_menos_alternativas(Perms_Possiveis, Escolha)----------- %

escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
	exclude(perm_unitaria, Perms_Possiveis, Nao_Unitarias),
	Nao_Unitarias \== [],
	menor_perm(Nao_Unitarias, Escolha).

perm_unitaria([_, Perms]) :- 
	length(Perms, Comp), Comp == 1.

menor_perm([[Vars, Perms]|R], Escolha) :-
	length(Perms, Comp),
	menor_perm_aux(R, [Vars, Perms], Escolha, Comp).

menor_perm_aux([], Escolha, Escolha, _) :- !.

menor_perm_aux([[Vars, Perms]|R], Atual, Escolha, CompAtual) :-
	length(Perms, Comp),
	Comp < CompAtual -> menor_perm_aux(R, [Vars, Perms], Escolha, Comp);
	menor_perm_aux(R, Atual, Escolha, CompAtual).

% ------experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)---- %

experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis) :-
	member(Perm, Lst_Perms),
	Esp = Perm,
	append(Inicio, [[Esp, Lst_Perms]|Fim], Perms_Possiveis),
	append(Inicio, [[Esp, [Perm]]|Fim], Novas_Perms_Possiveis).

% -------------resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)----------- % 

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
	escolhe_menos_alternativas(Perms_Possiveis, Escolha), !,
	experimenta_perm(Escolha, Perms_Possiveis, AposExp),
	simplifica(AposExp, AposSimp),
	resolve_aux(AposSimp, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
	simplifica(Perms_Possiveis, Novas_Perms_Possiveis).

% ---------------------------------resolve(Puz)------------------------------ %

resolve(Puzzle) :-
	inicializa(Puzzle, Perms_Possiveis),
	resolve_aux(Perms_Possiveis, _).

% --------------------------------------------------------------------------- %
% ----------------------------------ESTRUTURAS------------------------------- %
% --------------------------------------------------------------------------- %

% ------------------------------------espaco--------------------------------- %

% cria_espaco(Soma, Vars, Espaco) - construtor

cria_espaco(Soma, Vars, espaco(Soma, Vars)).

% soma_espaco(Espaco, Soma) - seletor
soma_espaco(espaco(Soma, _), Soma).

% vars_espaco(Espaco, Vars) - seletor
vars_espaco(espaco(_, Vars), Vars).