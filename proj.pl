% Diogo Gaspar, 99207,

:- [codigo_comum].

% --------------------combinacoes_soma(N, Els, Soma, Combs)--------------------

combinacoes_soma(N, Els, Soma, Combs) :-
  findall(AuxComb, (combinacao(N, Els, AuxComb), 
					sumlist(AuxComb, Soma)), AuxCombs),
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
	espaco_fila(Fila, Esp, H_V, [], 0).

% caso o primeiro elemento da  nao seja uma lista
espaco_fila([P|R], Esp, H_V, VarsAtuais, SomaAtual) :-
	\+ is_list(P), !,
	append(VarsAtuais, [P], VarsAtualizadas),
	espaco_fila(R, Esp, H_V, VarsAtualizadas, SomaAtual).

% caso estejamos na presença da lista vazia
espaco_fila([], Esp, _, VarsAtuais, SomaAtual) :-
	length(VarsAtuais, Comp), Comp > 0, !,
	faz_espaco(SomaAtual, VarsAtuais, Esp).

% caso o primeiro elemento seja uma lista e nao seja o primeiro elemento da fila
espaco_fila([P|R], Esp, H_V, VarsAtuais, SomaAtual) :-
	is_list(P), % decidi incluir para o codigo ser mais claro
	length(VarsAtuais, Comp), Comp > 0, 
	faz_espaco(SomaAtual, VarsAtuais, Esp),
	acessa_indice(P, H_V, NovaSoma),
	espaco_fila(R, _, H_V, [], NovaSoma).

% caso o primeiro elemento seja uma lista e seja o primeiro elemento da fila
espaco_fila([P|R], Esp, H_V, _, _) :-
	acessa_indice(P, H_V, NovaSoma),
	espaco_fila(R, Esp, H_V, [], NovaSoma).

% predicado auxiliar - acessa_indice(L, H_V, Soma)

% predicado de decisao sobre qual elemento da lista aceder
acessa_indice(L, v, Soma) :-
	nth0(0, L, Soma).

acessa_indice(L, h, Soma) :-
	nth0(1, L, Soma).

% salvaguarda para inputs errados
acessa_indice(_, H_V, _) :-
	H_V \== v,
	H_V \== h,
	fail.

% ------------------------espacos_fila(Fila, Esp, H_V)-------------------------

espacos_fila(H_V, Fila, Espacos) :-
	findall(Esp, espaco_fila(Fila, Esp, H_V), Espacos).

% ---------------------------------------------------------------

% estruturas

% espaco

% faz_espaco(Soma, Vars, Espaco)

faz_espaco(Soma, Vars, espaco(Soma, Vars)).

% soma_espaco(Espaco, Soma)
soma_espaco(espaco(Soma, _), Soma).

% vars_espaco(Espaco, Vars)
vars_espaco(espaco(_, Vars), Vars).