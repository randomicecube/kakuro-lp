% Diogo Gaspar, 99207

:- [codigo_comum, puzzles_publicos].

% --------------------------------------------------------------------------- %
% ----------------------------PREDICADOS PRINCIPAIS-------------------------- %
% --------------------------------------------------------------------------- %

% --------------------combinacoes_soma(N, Els, Soma, Combs)------------------ %

combinacoes_soma(N, Els, Soma, Combs) :-
  findall(Comb, (combinacao(N, Els, Comb), sumlist(Comb, Soma)), AuxCombs),
	% para ordenar as sublistas pelo primeiro (0) elemento
	sort(0, <, AuxCombs, Combs).

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
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos),
	length(Espacos, Comp),
	Comp > 0, !.

espacos_fila(_, _, []).

% -----------------------espacos_puzzle(Fila, Esp, H_V)---------------------- %

espacos_puzzle(Puzzle, Espacos) :-
	mat_transposta(Puzzle, Transp),
	busca_espacos(Puzzle, Horizontal, h),
	busca_espacos(Transp, Vertical, v),
	append(Horizontal, Vertical, PorAlisar),
	flatten(PorAlisar, Espacos).

% predicado auxiliar para ir buscar todos os espacos de uma dada grelha
busca_espacos(L, Res, H_V) :- maplist(espacos_fila(H_V), L, Res).

% ------------espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)------------ %

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
  vars_espaco(Esp, VarsEsp),
	include(alguma_var(VarsEsp), Espacos, Aux),
	subtract(Aux, [Esp], Esps_com).

% predicado auxiliar, verifica se ha alguma variavel em comum entre um espaco
% e um conjunto de variaveis	
alguma_var(Vars, Espaco) :-
	vars_espaco(Espaco, VarsEsp),
	findall(Var, (member(Var, VarsEsp), pertence(Vars, Var)), Aux),
	length(Aux, CompAux),
	CompAux > 0.

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

permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) :-
	% descobrir os espacos com pos comuns entre Espacos e Esp
	espacos_com_posicoes_comuns(Espacos, Esp, ComunsEsp),
	% compilar todos os "sets" de Perms_soma com espacos que estejam em ComunsEsp
	bagof(Comum, (member(Comum, Perms_soma), nth0(0, Esp, SubSet),
								pertence(Esp, ComunsEsp)), ComunsPermsSoma),
	% seletor que vai buscar as variaveis de Esp
	vars_espaco(Esp, VarsEsp),
	

















% -----permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)---- %

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