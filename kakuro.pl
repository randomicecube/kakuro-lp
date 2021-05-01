% Diogo Gaspar, 99207

:- [codigo_comum, puzzles_publicos].

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
  % para ordenar as sublistas pelo primeiro (0) elemento
  sort(0, <, AuxPerms, Perms).

% -------------------------espaco_fila(Fila, Esp, H_V)----------------------- %

espaco_fila(Fila, Esp, H_V) :-
	% as novas variaveis sao a lista aux. para variaveis e a soma atual
	espaco_fila(Fila, Esp, H_V, [], -1). % -1 enquanto nao houver uma soma real

% quando chegamos ao fim da fila
espaco_fila([], Esp, _, VarsAtuais, SomaAtual) :-
	length(VarsAtuais, Comp), Comp > 0,
	faz_espaco(SomaAtual, VarsAtuais, Esp).

% quando e uma lista e vem depois de um espaco
espaco_fila([P|_], Esp, _, VarsAtuais, SomaAtual) :-
	is_list(P),
	length(VarsAtuais, Comp), Comp > 0,
	SomaAtual > -1,
	faz_espaco(SomaAtual, VarsAtuais, Esp).

% quando e uma variavel (parte do espaco, portanto)
espaco_fila([P|R], Esp, H_V, VarsAtuais, SomaAtual) :-
	var(P), !,
	append(VarsAtuais, [P], VarsAtualizadas),
	espaco_fila(R, Esp, H_V, VarsAtualizadas, SomaAtual).

% quando e o primeiro elemento da fila/uma lista depois de outra lista
espaco_fila([P|R], Esp, H_V, _, _) :-
	acessa_indice(P, H_V, NovaSoma),
	espaco_fila(R, Esp, H_V, [], NovaSoma).

% predicado auxiliar - acessa_indice(L, H_V, Soma)

% predicado de decisao sobre qual elemento da lista aceder
acessa_indice([Soma, _], v, Soma).

acessa_indice([_, Soma], h, Soma).

% salvaguarda para H_V's errados
acessa_indice(_, H_V, _) :-
	H_V \== v,
	H_V \== h.

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
	include(algum(VarsEsp), Espacos, Aux),
	subtract(Aux, [Esp], Esps_com).

algum(Vars, Espaco) :-
	vars_espaco(Espaco, VarsEsp),
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

% predicado auxiliar, lista de 1 até N, caso 1 <= N < 9; de 1 a 9 caso N >= 9
faz_lista(N, L) :-
	N < 9 -> findall(Iterador, between(1, N, Iterador), L);
	findall(Iterador, between(1, 9, Iterador), L).

% ---------permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)-------- %

% a ideia por detras disto e a unificacao da variavel ocorrer na "scope toda",
% ou seja acaba por ser possivel fazer a comparacao com a unificacao atual e
% ver se todas continuam a unificar

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	% descobrir os espacos com pos comuns entre Espacos e Esp
	espacos_com_posicoes_comuns(Espacos, Esp, EspsComuns),
	% compilar todas os "sets" de Perms_soma dos espacos comuns
	include(verifica_comuns(EspsComuns), Perms_soma, SetsComuns),
	% obter o "set" do espaco Esp
	permutacoes_soma_espacos([Esp], SetEspAux),
	% alisar SetEsp
	append(SetEspAux, SetEsp),
	% obter as permutacoes possiveis para o espaco Esp
	nth0(1, SetEsp, PermsEsp),
	% obter as variaveis do espaco Esp
	vars_espaco(Esp, VarsEsp),
	% obter uma permutacao-membro das permutacoes de Esp
	member(PermPossivel, PermsEsp),
	% unificar as variaveis do espaco Esp com essa permutacao-membro
	VarsEsp = PermPossivel,
	verifica_unifica(SetsComuns),
	% caso possa, Perm unifica com PermPossivel
	Perm = PermPossivel.

% predicado auxiliar que recebe uma lista de espacos e um "set" de Perms_soma
% e verifica se o espaco desse set pertence a lista
verifica_comuns(Esps, Set) :-
	nth0(0, Set, Esp),
	pertence(Esps, Esp).

% predicado auxiliar que verifica se com a substituicao de variaveis proposta
% ha pelo menos uma permutacao que mantem a permutacao original certa
verifica_unifica([]) :- !.

verifica_unifica([P|R]) :-
	nth0(0, P, Esp),
	vars_espaco(Esp, VarsEsp),
	nth0(1, P, Perms),
	bagof(Perm, (member(Perm, Perms), subsumes_term(VarsEsp, Perm)), _),
	% caso haja pelo menos uma, continua; caso contrario, verifica_unifica falha
	verifica_unifica(R).


% -----permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)---- %

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [VarsEsp, Perms]) :-
	vars_espaco(Esp, VarsEsp),
	bagof(Perm, permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), Perms).

% -----------permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)--------- %

% -----------------numeros_comuns(Lst_Perms, Numeros_comuns)----------------- %

numeros_comuns(Lst_Perms, Numeros_comuns) :-
	% a tranposta sera utilizada para facilitar a comparacao
	mat_transposta(Lst_Perms, Transp),
	findall((Pos, N), (member(SubL, Transp), list_to_set(SubL, [_]), 
					nth1(Pos, Transp, SubL), nth1(1, SubL, N)), Numeros_comuns).

% ----------------------atribui_comuns(Perms_possiveis)---------------------- %

% ---------retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis--------- %

% -------------simplifica(Perms_Possiveis, Novas_Perms_Possiveis------------- %

% --------------------inicializa(Puzzle, Perms_Possiveis)-------------------- %

% ------------escolhe_menos_alternativas(Perms_Possiveis, Escolha)----------- %

% ------experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)---- %

% -------------resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)----------- %

% ---------------------------------resolve(Puz)------------------------------ %

% --------------------------------------------------------------------------- %
% ----------------------------------ESTRUTURAS------------------------------- %
% --------------------------------------------------------------------------- %

% ------------------------------------espaco--------------------------------- %

% faz_espaco(Soma, Vars, Espaco) - construtor

faz_espaco(Soma, Vars, espaco(Soma, Vars)).

% soma_espaco(Espaco, Soma) - seletor
soma_espaco(espaco(Soma, _), Soma).

% vars_espaco(Espaco, Vars) - seletor
vars_espaco(espaco(_, Vars), Vars).