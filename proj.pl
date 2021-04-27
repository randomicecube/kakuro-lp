% Diogo Gaspar, 99207

:- [codigo_comum].

% ----------------------------PREDICADOS PRINCIPAIS----------------------------

% --------------------combinacoes_soma(N, Els, Soma, Combs)--------------------

combinacoes_soma(N, Els, Soma, Combs) :-
  findall(AuxComb, (combinacao(N, Els, AuxComb), 
					sumlist(AuxComb, Soma)), AuxCombs),
	% para ordenar as sublistas pelo primeiro (0) elemento
	sort(0, <, AuxCombs, Combs).

% --------------------permutacoes_soma(N, Els, Soma, Combs)--------------------

permutacoes_soma(N, Els, Soma, Perms) :-
  combinacoes_soma(N, Els, Soma, Combs),
  findall(CombPer, (member(Comb, Combs), permutation(Comb, CombPer)), AuxPerms),
  % para ordenar as sublistas pelo primeiro (0) elemento
  sort(0, <, AuxPerms, Perms).

% -------------------------espaco_fila(Fila, Esp, H_V)-------------------------

espaco_fila(Fila, Esp, H_V) :-
	% as novas variaveis sao a lista aux. para variaveis e a soma atual
	espaco_fila(Fila, Esp, H_V, [], -1). % -1 enquanto nao houver uma soma real

% quando chegamos ao fim da fila
espaco_fila([], Esp, _, VarsAtuais, SomaAtual) :-
	length(VarsAtuais, Comp), Comp > 0,
	SomaAtual > -1,
	faz_espaco(SomaAtual, VarsAtuais, Esp).

% quando e uma lista e vem depois de um espaco
espaco_fila([P|_], Esp, _, VarsAtuais, SomaAtual) :-
	is_list(P),
	length(VarsAtuais, Comp), Comp > 0,
	SomaAtual > -1,
	faz_espaco(SomaAtual, VarsAtuais, Esp).

% quando e uma variavel (parte do espaco, portanto)
espaco_fila([P|R], Esp, H_V, VarsAtuais, SomaAtual) :-
	\+ is_list(P), !,
	append(VarsAtuais, [P], VarsAtualizadas),
	espaco_fila(R, Esp, H_V, VarsAtualizadas, SomaAtual).

% quando e o primeiro elemento da fila/uma lista depois de outra lista
espaco_fila([P|R], Esp, H_V, _, _) :-
	acessa_indice(P, H_V, NovaSoma),
	espaco_fila(R, Esp, H_V, [], NovaSoma).

% predicado auxiliar - acessa_indice(L, H_V, Soma)

% predicado de decisao sobre qual elemento da lista aceder
acessa_indice(L, v, Soma) :-
	nth0(0, L, Soma).

acessa_indice(L, h, Soma) :-
	nth0(1, L, Soma).

% salvaguarda para H_V's errados
acessa_indice(_, H_V, _) :-
	H_V \== v,
	H_V \== h,
	fail.

% ------------------------espacos_fila(Fila, Esp, H_V)-------------------------

espacos_fila(H_V, Fila, Espacos) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos),
	length(Espacos, Comp),
	Comp > 0, !.

% caso a fila nao tenha espacos possiveis
espacos_fila(_, _, []).

% -----------------------espacos_puzzle(Fila, Esp, H_V)------------------------

espacos_puzzle(Puzzle, Espacos) :-
	mat_transposta(Puzzle, Transp),
	busca_espacos(Puzzle, Horizontal, h),
	busca_espacos(Transp, Vertical, v),
	append(Horizontal, Vertical, PorAlisar),
	flatten(PorAlisar, Espacos).

% predicado auxiliar para ir buscar todos os espacos de uma dada grelha
busca_espacos(L, Res, H_V) :-
	maplist(espacos_fila(H_V), L, Res).

% ------------espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)--------------

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
  vars_espaco(Esp, VarsEsp),
	include(algum(VarsEsp), Espacos, Aux),
	subtract(Aux, [Esp], Esps_com).
	
algum(Vars, Espaco) :-
	vars_espaco(Espaco, VarsEsp),
	findall(Var, (member(Var, VarsEsp), pertence(Vars, Var)), Aux),
	length(Aux, CompAux),
	CompAux > 0.

% predicado auxiliar, semelhante ao member/2 mas sem a unificacao
pertence([P|_], X) :- X == P, !.

pertence([_|R], X) :- pertence(R, X).
% ----------------------------------ESTRUTURAS---------------------------------

% ------------------------------------espaco-----------------------------------

% faz_espaco(Soma, Vars, Espaco) - construtor

faz_espaco(Soma, Vars, espaco(Soma, Vars)).

% soma_espaco(Espaco, Soma) - seletor
soma_espaco(espaco(Soma, _), Soma).

% vars_espaco(Espaco, Vars) - seletor
vars_espaco(espaco(_, Vars), Vars).