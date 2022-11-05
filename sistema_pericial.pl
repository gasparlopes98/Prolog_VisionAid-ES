% Versão preparada para lidar com regras que contenham negação (nao)
% Metaconhecimento
% Usar base de conhecimento veIculos2.txt
% asdsada
% Explicações como?(how?) e porque não?(whynot?)

:-op(220,xfx,entao).
:-op(35,xfy,se).
:-op(240,fx,regra).
:-op(500,fy,nao).
:-op(600,xfy,e).
:-op(700,xfy,ou).

:-dynamic justifica/3.


debug(N):- write(N),write('\n').
debug_message(M,N):- write(M), write(": "),write(N),write('\n').

carrega_bc:-
		write('=== Base de Conhecimentos Carregada ==='),nl,
		consult("rl.txt").
%		write('NOME DA BASE DE CONHECIMENTO (terminar com .)-> '),
%		read(NBC),
%		consult(NBC).

help:-
	write('- Para Carregar a BC: carrega_bc'),nl,
	write('- Para Inserir Objeto: inserir_objeto'),nl,
	write('- Para Arrancar o MI: arranca_motor'),nl,
	write('- Para Mostrar Factos: mostra_factos'),nl,
	write('- Para Perceber Facto (N - número de facto): como(N)'),nl,
	write('- Para Perceber Facto Não Acionado ex.: whynot(lens(nome_obj, tamanho)).'),nl.

arranca_motor:-	facto(N,Facto),
		facto_dispara_regras1(Facto, LRegras),
		dispara_regras(N, Facto, LRegras),
		ultimo_facto(N).

facto_dispara_regras1(Facto, LRegras):-
	facto_dispara_regras(Facto, LRegras),
	!.
facto_dispara_regras1(_, []).

% Caso em que o facto não origina o disparo de qualquer regra.
dispara_regras(N, Facto, [ID|LRegras]):-
	regra ID se LHS entao RHS,
	facto_esta_numa_condicao(Facto,LHS),
% Instancia Facto em LHS
	verifica_condicoes(LHS, LFactos),
	member(N,LFactos),
	concluir(RHS,ID,LFactos),
	!,
	dispara_regras(N, Facto, LRegras).

dispara_regras(N, Facto, [_|LRegras]):-
	dispara_regras(N, Facto, LRegras).

dispara_regras(_, _, []).


facto_esta_numa_condicao(F,[F  e _]).

facto_esta_numa_condicao(F,[F  ou _]).

facto_esta_numa_condicao(F,[avalia(F1)  e _]):- F=..[H,H1|_],F1=..[H,H1|_].

facto_esta_numa_condicao(F,[avalia(F1)  ou _]):- F=..[H,H1|_],F1=..[H,H1|_].

facto_esta_numa_condicao(F,[_ e Fs]):- facto_esta_numa_condicao(F,[Fs]).

facto_esta_numa_condicao(F,[_ ou Fs]):- facto_esta_numa_condicao(F,[Fs]).

facto_esta_numa_condicao(F,[F]).

facto_esta_numa_condicao(F,[avalia(F1)]):-F=..[H,H1|_],F1=..[H,H1|_].


verifica_condicoes([nao avalia(X) e Y],[nao X|LF]):- !,
	\+ avalia(_,X),
	verifica_condicoes([Y],LF).
verifica_condicoes([avalia(X) e Y],[N|LF]):- !,
	avalia(N,X),
	verifica_condicoes([Y],LF).

verifica_condicoes([nao avalia(X) ou Y],[nao X|LF]):- !,
	\+ avalia(_,X),
	verifica_condicoes([Y],LF).

verifica_condicoes([avalia(X) ou Y],[N|LF]):- !,
	avalia(N,X),
	verifica_condicoes([Y],LF).	

verifica_condicoes([nao avalia(X)],[nao X]):- !, \+ avalia(_,X).
verifica_condicoes([avalia(X)],[N]):- !, avalia(N,X).

verifica_condicoes([nao X e Y],[nao X|LF]):- !,
	\+ facto(_,X),
	verifica_condicoes([Y],LF).


verifica_condicoes([X e Y],[N|LF]):- !,
	facto(N,X),
	verifica_condicoes([Y],LF).

verifica_condicoes([nao X ou Y],[nao X|LF]):- !,
	\+ facto(_,X),
	verifica_condicoes([Y],LF).

verifica_condicoes([X ou _],[N|_]):- 
	facto(N,X).

verifica_condicoes([_ ou Y],[_|LF]):- 
	verifica_condicoes([Y],LF).		

verifica_condicoes([nao X],[nao X]):- !, \+ facto(_,X).
verifica_condicoes([X],[N]):- facto(N,X).



concluir([cria_facto(F)|Y],ID,LFactos):-
	!,
	cria_facto(F,ID,LFactos),
	concluir(Y,ID,LFactos).

concluir([],_,_):-!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Inserir Objeto

condicao1(Facto,Nome,String):-
	findall(Facto, facto(_,Facto), LFactos),
	length(LFactos,Leng),
	Leng>0,
	write(String),
	read(Result),
	cria_facto1(characteristicMaterial(Nome,Result)).

condicao2(Facto,Nome,String):-
	findall(Facto, facto(_,Facto), LFactos),
	length(LFactos,Leng),
	Leng>0,
	write(String),
	read(Result),
	cria_facto1(typeLight(Nome,Result)).

inserir_objeto:-
	write('Nome do Objeto:'),
	read(Nome),
	write('Qual o tamanho do objeto'),
	read(Size),
	write('Tipo de material?'),
	read(Tipo),
	write('Está em movimento? (sim/nao)'),
	read(Mov),
	write('O que quer inspecionar?'),
	read(Highlight),
	write('Luz externa? (sim/nao)'),
	read(Ext),
	cria_facto1(size(Nome,Size)),
	cria_facto1(material(Nome,Tipo)),
	cria_facto1(motion(Nome,Mov)),
	cria_facto1(highlight(Nome,Highlight)),
	cria_facto1(externalLight(Nome,Ext)),
	condicao1(highlight(Nome,colour),Nome,"Caracteristica do material?"),
	condicao2(externalLight(Nome,yes),Nome,"Qual o tipo de luz?").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Criar Facto

cria_facto1(F):-
	retract(ultimo_facto(N1)),
	N is N1+1,
	asserta(ultimo_facto(N)),
	assertz(facto(N,F)),
	write('O facto numero '),write(N),write(' foi concluído -> '),write(F),get0(_),!.

cria_facto(F,_,_):-
	facto(_,F),!.

cria_facto(F,ID,LFactos):-
	retract(ultimo_facto(N1)),
	N is N1+1,
	asserta(ultimo_facto(N)),
	assertz(justifica(N,ID,LFactos)),
	assertz(facto(N,F)),
	write('O facto numero '),write(N),write(' foi concluído -> '),write(F),get0(_),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

avalia(N,P):-	P=..[Functor,Entidade,Operando,Valor],
		P1=..[Functor,Entidade,Valor1],
		facto(N,P1),
		compara(Valor1,Operando,Valor).

compara(V1,==,V):- V1==V.
compara(V1,\==,V):- V1\==V.
compara(V1,>,V):-V1>V.
compara(V1,<,V):-V1<V.
compara(V1,>=,V):-V1>=V.
compara(V1,=<,V):-V1=<V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Visualização da base de factos

mostra_factos:-
	findall(N, facto(N, _), LFactos),
	escreve_factos(LFactos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geração de explicações do tipo "Como"

como(N):-ultimo_facto(Last),Last<N,!,
	write('A conclusão não foi encontrada'),nl,nl.
como(N):-justifica(N,ID,LFactos),!,
	facto(N,F),
	write('Conclui o facto número '),write(N),write(' -> '),write(F),nl,
	write('com a regra '),write(ID),nl,
	write('depois de verificar:'),nl,
	escreve_factos(LFactos),
	write('********************************************************'),nl,
	explica(LFactos).
como(N):-facto(N,F),
	write('O facto numero '),write(N),write(' -> '),write(F),nl,
	write('foi inicialmente conhecido'),nl,
	write('********************************************************'),nl,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Escreve factos -> chamado no como e no mostra_factos

escreve_factos([I|R]):-facto(I,F), !,
	write('O facto numero '),write(I),write(' -> '),write(F),write(' é verdadeira'),nl,
	escreve_factos(R),!.
escreve_factos([I|R]):-
	write('A condição '),write(I),write(' é verdadeira'),nl,
	escreve_factos(R).
escreve_factos([]).

explica([I|R]):- \+ integer(I),!,explica(R).
explica([I|R]):-como(I),
		explica(R).
explica([]):-	write('********************************************************'),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Geração de explicações do tipo "Porque nao"

whynot(Facto):-
	whynot(Facto,1).

whynot(Facto,_):-
	facto(_, Facto),
	!,
	write('O facto '),write(Facto),write(' não é falso!'),nl.
whynot(Facto,Nivel):-
	encontra_regras_whynot(Facto,LLPF),
	whynot1(LLPF,Nivel).
whynot(nao Facto,Nivel):-
	formata(Nivel),write('Porque:'),write(' O facto '),write(Facto),
	write(' é verdadeiro'),nl.
whynot(Facto,Nivel):-
	formata(Nivel),write('Porque:'),write(' O facto '),write(Facto),
	write(' não é definido na base de conhecimento'),nl.

%  As explicações do whynot(Facto) devem considerar todas as regras que poderiam dar origem a conclusão relativa ao facto Facto

encontra_regras_whynot(Facto,LLPF):-
	findall((ID,LPF),
		(
		regra ID se LHS entao RHS,
		member(cria_facto(Facto),RHS),
		encontra_premissas_falsas(LHS,LPF),
		LPF \== []
		),
		LLPF).

whynot1([],_).
whynot1([(ID,LPF)|LLPF],Nivel):-
	formata(Nivel),write('Porque pela regra '),write(ID),write(':'),nl,
	Nivel1 is Nivel+1,
	explica_porque_nao(LPF,Nivel1),
	whynot1(LLPF,Nivel).

encontra_premissas_falsas([nao X e Y], LPF):-
	verifica_condicoes([nao X], _),
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([X e Y], LPF):-
	verifica_condicoes([X], _),
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([nao X], []):-
	verifica_condicoes([nao X], _),
	!.
encontra_premissas_falsas([X], []):-
	verifica_condicoes([X], _),
	!.
encontra_premissas_falsas([nao X e Y], [nao X|LPF]):-
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([X e Y], [X|LPF]):-
	!,
	encontra_premissas_falsas([Y], LPF).
encontra_premissas_falsas([nao X], [nao X]):-!.
encontra_premissas_falsas([X], [X]).
encontra_premissas_falsas([]).

explica_porque_nao([],_).
explica_porque_nao([nao avalia(X)|LPF],Nivel):-
	!,
	formata(Nivel),write('A condição '),write(X),write(' é falsa'),nl,
	explica_porque_nao(LPF,Nivel).
explica_porque_nao([avalia(X)|LPF],Nivel):-
	!,
	formata(Nivel),write('A condição '),write(X),write(' é falsa'),nl,
	explica_porque_nao(LPF,Nivel).
explica_porque_nao([P|LPF],Nivel):-
	formata(Nivel),write('A premissa '),write(P),write(' é falsa'),nl,
	Nivel1 is Nivel+1,
	whynot(P,Nivel1),
	explica_porque_nao(LPF,Nivel).

formata(Nivel):-
	Esp is (Nivel-1)*5, tab(Esp).