%PARTE 1

%----------
%BASE DE CONHECIMENTO
%----------

:-style_check(-discontiguous).
:-style_check(-singleton).

:- op(900,xfy,'::').
:-dynamic(utente/10).
:-dynamic(centro_saude/5).
:-dynamic(staff/4).
:-dynamic(vacinacao_covid/5).
:-dynamic('-'/1).


%---------
% Extensão do predicado utente: #Idutente, Nº Segurança_Social, Nome,
% Data_Nasc, Email, Telefone, Morada,  [Doenças_Crónicas],Profissão,
% #CentroSaúde ↝ { 𝕍, 𝔽}

utente(1, 23456, 'Carlos', 24-04-1999,'carlos@gmail.com', '966345781','Rua do Falcão',[Renite],'Professor',1).
utente(2, 23906, 'Rui', 13-03-1987,'rui@gmail.com', '989349235','Rua da Esquina',[],'Médico',1).
utente(3, 12396, 'Carlota', 20-09-2005,'carlota@gmail.com', '963215637','Rua Alto Mar',[],'Estudante',1).
utente(4, 76590, 'Maria', 25-09-1935,'maria@gmail.com', '934674129','Rua do Bairro',[],'Carpinteira',1).
utente(5, 56851, 'Jorge', 29-05-1955,'jorge@gmail.com', '962458965','Rua de S.Bento da Várzea',[],'Psicóloga',1).
utente(6, 65975, 'Alexandra', 18-02-2009,'alexandra@gmail.com', '912358957','Rua Alto Mar',[],'Enfermeira',2).
utente(10, 75362,'Lisandro',05-06-2005,'lisandro@gmail.com', '963030147', 'Rua Lisboa',[],'Estudante',2).



%utente candidata à vacinação na fase 1 que ainda nao foi vacinado
utente(7, 45210, 'Cristiana', 18-08-2012,'cristiana@gmail.com', '932013663','Rua dos Peões',[Alergias, Bronquite],'Tradutor',2).


utente(8, 78563, 'Sílvio', 14-11-2000,'silvio@gmail.com', '932456901','Rua António Cardodo',[],'Eletricista',2).
utente(9, 96203, 'Rafael', 03-10-1977,'rafael@gmail.com', '963124032','Rua da Boa Vontade',[],'Engenheira',2).


%-----------------------------------------------------------------
%Extensão do predicado centro_saude: #Idcentro, Nome, Morada, Telefone, Email ↝ { 𝕍, 𝔽}

centro_saude(1,'Centro de Saúde de Barcelos', 'Rua Alta', '253234674', 'centro1@gmail.com').
centro_saude(2,'Hospital De Braga', 'Rua das Paredes ', '253523489', 'centro2@gmail.com').

%-----------------------------------------------------------------
%EXtensão do predicado staff: #Idstaff, #Idcentro, Nome, email l ↝ { 𝕍, 𝔽 }

staff(1,1,'Rute Silva', 'rute@gmail.com').
staff(2,1,'Santiago Rua', 'santiago@gmail.com').
staff(3,2,'Vitor Castro', 'vitor@gmail.com').
staff(4,2,'Catarina Coutada', 'catarina@gmail.com').

%-----------------------------------------------------------------
%Extensão do predicado vacinacao_covid: #Staf, #utente, Data, Vacina, Toma↝ { 𝕍, 𝔽 }

vacinacao_covid(1,2,6-3-2021, 'Vacina2',1).
vacinacao_covid(2,5,6-1-2021, 'Vacina1', 1).

%utente da fase 1, bem vacinado que só tem a primeira toma
vacinacao_covid(2,1,20-04-2021, 'Vacina1',1).

%utentes mal vacinado que tem ambas as tomas da vacina
vacinacao_covid(2,3,05-02-2021,'Vacina1',1).
vacinacao_covid(2,3,05-04-2021, 'Vacina1',2).

%utentes da fase 2 que foi mal vacinado dentro das datas da fase 1
vacinacao_covid(3,10,12-01-2021,'Vacina1',1).

%utente bem vacinado da fase 2
vacinacao_covid(4,8,05-08-2021, 'Vacina2',1).

%-----------------------------------------------------------------
%Extensão do predicado para explorar a base de conhecimento

resposta(X,Y,Z):- findall(X,Y,Z).

%-----------------------------------------------------------------
%Obter utente por critério de seleção

utenteID(ID,R) :- resposta((ID,A,B,C,D,E,F,G,H,I),utente(ID,A,B,C,D,E,F,G,H,I),R).
utenteSocial(SO,R) :- resposta((ID,SO,B,C,D,E,F,G,H,I),utente(ID,SO,B,C,D,E,F,G,H,I),R).
utenteNome(NO,R) :- resposta((ID,A,NO,C,D,E,F,G,H,I),utente(ID,A,NO,C,D,E,F,G,H,I),R).
utenteNascimento(DA,R) :- resposta((ID,A,B,DA,D,E,F,G,H,I),utente(ID,A,B,DA,D,E,F,G,H,I),R).
utenteEmail(EM,R) :- resposta((ID,A,B,C,EM,E,F,G,H,I),utente(ID,A,B,C,EM,E,F,G,H,I),R).
utenteTelemovel(TE, R) :- resposta((ID,A,B,C,D,TE,F,G,H,I),utente(ID,A,B,C,D,TE,F,G,H,I),R).
utenteMorada(MO,R) :- resposta((ID,A,B,C,D,E,MO,G,H,I),utente(ID,A,B,C,D,E,MO,G,H,I),R).
utenteDoencas(DO,R) :- resposta((ID,A,B,C,D,E,F,DO,H,I),utente(ID,A,B,C,D,E,F,DO,H,I),R).
utenteCentro(CE,R) :- resposta((ID,A,B,C,D,E,F,G,H,CE),utente(ID,A,B,C,D,E,F,G,H,CE),R).

%--------------------------------------------------------------------
%Critérios da 1 Fase de vacinação
%vacinação começa em janeiro (Mes 01) e acaba em MArço(Mes 03)

%devolve os utentes candidatos à fase 1
fase1(R) :- idosos(I), doentes(D), concat(I,D,L),profmedico(M),concat(M,L,X),profenfermeiro(J),concat(X,J,Y),profenfermeira(W), concat(W,Y,Z), profmedica(C), concat(Z,C,S),repetidos(S,R).

%devolve a lista de utentes que tem mais de 50 anos
idosos(R) :- resposta(U, (utente(U,_,_,D-M-A,_,_,_,_,_,_),X is 2021 - A, X>50), R).


%devolve lista de utentes que tem doenças
doentes(R) :- resposta(U, (utente(U,_,_,_,_,_,_,L,_,_), comprimento(L,T), T>0),R).


%devolve a lista de utentes que tem como profissão médico ou enfermeiro
profmedico(R) :- resposta(U, utente(U,_,_,_,_,_,_,_,'Médico',_),R).
profenfermeiro(R) :- resposta(U, utente(U,_,_,_,_,_,_,_,'Enfermeiro',_),R).
profenfermeira(R) :- resposta(U, utente(U,_,_,_,_,_,_,_,'Enfermeira',_),R).
profmedica(R) :- resposta(U, utente(U,_,_,_,_,_,_,_,'Médica',_),R).

%--------------------------------------------------------------------
%Critérios da 2 Fase de vacinação

%devolve os utentes candidatos à fase 2
fase2(R) :- resposta((U),(utente(U,_,_,_,_,_,_,_,_,_), fase1(R),
		nao(pertence(U,R))),L), repetidos(L,R).

%-----------------------------------------------------------------
%Identificar utentes vacinados

vacinados(R) :- resposta(U, utente(U,_,_,_,_,_,_,_,_,_),L),
		              utenteVac(L,R).

utenteVac([U],R) :- utenteVacinado(U,R).
utenteVac([U|US],R) :- utenteVacinado(U,F),
			utenteVac(US,P), 
			concat(F,P,R).

utenteVacinado(U,R) :- resposta(U, vacinacao_covid(_,U,_,_,_),L), 
		       repetidos(L,R).


repetidos([],[]).
repetidos([X|A],R) :- retiraEle(X,A,L),
                      repetidos(L,T),
                      R = [X|T].

retiraEle(A,[],[]).
retiraEle(A,[A|Y],T) :- retiraEle(A,Y,T).
retiraEle(A,[X|Y],T) :- X \== A,
                        retiraEle(A,Y,R),
                        T = [X|R].

concat([],L2,L2).
concat(L1,[],L1).
concat([X|L1],L2,[X|L]) :- concat(L1,L2,L).

%--------------------------------------------------------------------
%Identificar utentes não vacinados

nao( Questao ) :- Questao, !, fail.
nao( _ ).

pertence( X,[X|_] ).
pertence( X,[Y|L] ) :- X \= Y, pertence( X,L ).

naovac(R) :- resposta((U,A,B,D,F),(utente(U,A,B,_,D,_,F,_,_,_), vacinados(R),nao(pertence(U,R))),L),
		repetidos(L,R).

%--------------------------------------------------------------------
%Identificar pessoas vacinadas indevidamente
%Partimos do principio que a vacinação começa em Janeiro (01)
%Logo um utente é vacinado indevidamente quando é vacinado entre janeiro e Março , sem estar nos candidatos desta fase

mal(R) :- resposta((U,NS,N), (utente(U,NS,N,_,_,_,_,_,_,_),vacinados(V), pertence(U,V), vacinadosDentroData1fase(Y), pertence(U,Y),fase1(X), nao(pertence(U,X))),L), repetidos(L,R).

vacinadosDentroData1fase(R) :- resposta(U,(utente(U,_,_,_,_,_,_,_,_,_),vacinacao_covid(_,U,D-M-A,_,_), M>=1, M<4),R).

%--------------------------------------------------------------------
%Identificar pessoas não vacinadas e que são candidatas a vacinação

candidatas(R) :- (resposta(U,(utente(U,_,_,_,_,_,_,_,_,_), naovacID(X),fase1(V),pertence(U,X),pertence(U,V)),L),repetidos(L,R)).


intersecao(P,S,L) :- subtract(P,S,A), subtract(P,A,B),sort(B,L).

naovacID(R) :- resposta(U,(utente(U,_,_,_,_,_,_,_,_,_), vacinados(R),nao(pertence(U,R))),L),
    repetidos(L,R).

common_elements([H|_], L2) :-
    membereq(H, L2).
common_elements([_|T], L2) :-
    common_elements(T, L2).

membereq(X, [H|_]) :-
    X == H.
membereq(X, [_|T]) :-
    membereq(X, T).

%--------------------------------------------------------------------
%Identificar pessoas a quem falta a segunda toma da vacina 

primeiratoma(R) :- resposta((U,N),(utente(U,_,N,_,_,_,_,_,_,_), vacinacao_covid(_,U,_,_,1),nao(vacinacao_covid(_,U,_,_,2))),R).

%------------------------------------------------------------
%Sistema de inferência

demo(Questao, falso) :- -Questao.
demo(Questao,verdadeiro) :- Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%-----------------------------------------------------------------
%Aumentar a base de conhecimento
%Registar novos utentes

registaUtente(ID,A,B,C,D,E,F,G,H,I) :- evolucao(utente(ID,A,B,C,D,E,F,G,H,I)).


evolucao(T) :- resposta(I, +T::I, L),
               insercao(T),
               teste(L).

insercao(T) :- assert(T).
insercao(T) :- retract(T), !, fail.

teste( [] ).
teste( [R|LR] ) :- R, teste( LR ).


%Nao permite que seja adicionada informação repetida (utente com o mesmo ID)
+utente(ID,A,B,C,D,E,F,G,H,I)::(resposta(ID,(utente(ID,_,_,_,_,_,_,_,_,_)),L),
                                comprimento(L,R),
                                R==1).

%Nao deixa dois utentes ter o mesmo número de segurança social
+utente(ID,S,B,C,D,E,F,G,H,I)::(resposta(S,(utente(_,S,_,_,_,_,_,_,_,_)),L),
                                comprimento(L,R),
                                R==1).

comprimento([], 0).
comprimento([_|T], R) :- comprimento(T, N), R is N+1.

%------------------------------------------------------------------------
%Registar novo staff

registaStaff(A,B,C,D) :- evolucao(staff(A,B,C,D)).


%Não permte que seja adicionada informação repetida (staff com o mesmo ID)
+staff(A,B,C,D)::(resposta(A,(staff(A,_,_,_)), L),
                  comprimento(L,R),
                  R==1).

%------------------------------------------------------------------------
%Registar novos centros

registaCentro(A,B,C,D,E) :- evolucao(centro_saude(A,B,C,D,E)).

%Não permte que seja adicionada informação repetida (centro com o mesmo ID)
+centro_saude(A,B,C,D,E)::(resposta(A, (centro_saude(A,_,_,_,_)), L),
                          comprimento(L,R),
                          R==1).

%------------------------------------------------------------------------
%Registar novos vacinados

registaVacina(A,B,C,D,E) :-evolucao(vacinacao_covid(A,B,C,D,E)).



%A primeira toma é antes da segunda
%Se o registo for da segunda toma obtemos a data da primeira dose e testamos através do "testaData" se a dose introduzida é posterior.
+vacinacao_covid(_,U,D-M-A,_,T)::(T == 1; T == 2, (vacinacao_covid(_,U,D1-M1-A1,_,1),
                                  testaData(D,M,A,D1,M1,A1))).


%Apenas dois registos por utente
%Através de um "resposta" verifica quantos registos o utente tem na base de conhecimento e não permite que seja maior que 2.
+vacinacao_covid(_,U,_,_,T)::((resposta((U, T),(vacinacao_covid(_,U,_,_,T)),L),
                              comprimento(L,N),
                              N =< 2)).

testaData(D,M,A,D1,M1,A1) :- (A > A1);(A == A1,M > M1);(A == A1,M == M1,D>D1).


%-----------------------------------------------------------------------
%Remover utentes
%Ao remover um utente também removemos as vacinações registadas desse mesmo utente

removeUtente(U) :-  (resposta((A,U,B,C,D),(vacinacao_covid(A,U,B,C,D)),R),
                    removeVacinacoes(R),
                    retroceder(utente(U,_,_,_,_,_,_,_,_,_))).


removeVacinacoes([]).
removeVacinacoes([(A,U,B,C,D)|US]) :- removeVacinacao(U), removeVacinacoes(US).


retroceder(E) :- resposta(I,-E::I,L), 
                teste(L),               
                 remove(E).


remove(T) :- retract(T).
remove(T) :- assert(T),!,fail.

%------------------------------------------------------------------------
% Remove staff

removeStaff(ID) :- retroceder(staff(ID,B,C,D)).


%------------------------------------------------------------------------
% Remove centros

removeCentros(ID) :- retroceder(centro_saude(ID,B,C,D,E)).

%---------------------------------------------------------------------
% Remove registos de vacinados

removeVacinacao(ID):- retroceder(vacinacao_covid(A,ID,B,C,D)).

%EXTRA
%---------------------------------------------
% Número de utentes por centro_saude

utentePorCentro(C, R) :- (resposta((U), (utente(U,_,_,_,_,_,_,_,_,C)),L),
                          comprimento(L,R)).

%------------------------------------------------
% Utentes vacinados por um staff

utentePorStaff(S, R) :- (resposta((ID,N),(vacinacao_covid(S,ID,_,_,_), utente(ID,_,N,_,_,_,_,_,_,_)),L),repetidos(L,R)).

%----------------------------------------------------
%Utentes que já terminaram a fase de vacinação

terminou(R) :- resposta((U,N), (utente(U,_,N,_,_,_,_,_,_,_),vacinados(V), pertence(U,V), vacinacao_covid(_,U,_,_,2)),L),repetidos(L,R).

%-------------------------------------------------
%PARTE 2
%-----------------------------------------------
%CONHECIMENTO NEGATIVO

-utente(A,B,C,D,E,F,G,H,I,J) :- nao(utente(A,B,C,D,E,F,G,H,I,J)), 
                                nao(excecao(utente(A,B,C,D,E,F,G,H,I,J))).

-centro_saude(A,B,C,D,E) :- nao(centro_saude(A,B,C,D,E)), 
                            nao(excecao(centro_saude(A,B,C,D,E))).

-staff(A,B,C,D) :- nao(staff(A,B,C,D)), 
                   nao(excecao(staff(A,B,C,D))).

-vacinacao_covid(A,B,C,D,E) :- nao(vacinacao_covid(A,B,C,D,E)), 
                               nao(excecao(staff(A,B,C,D,E))).


%Um utente que tem o número de id 11 mas a única informação que sabemos do centro de saúde é que não é o 1
utente(11, 56320, 'Jonas', 23-09-1984, 'jonas@gmail.com', '965874301', 'Rua do Gota', [], 'Dermatologista', centro_desconhecido).
-utente(11, 56320, 'Jonas', 23-09-1984, 'jonas@gmail.com', '965874301', 'Rua do Gota', [], 'Dermatologista', 1).

%Um centro de sáude de id 3 mas a unica informação que sabemos do email é que não é 'centro1@gmail.com'
centro_saude(3,'Centro Santo António', 'Rua dos Sabores', '253523369', email_desconhecido).
-centro_saude(3,'Centro Santo António', 'Rua dos Sabores', '253523369', 'centro1@gmail.com').

%Um staff de id 5 mas que a unica informação que sabemos do email é que não é 'rui@gmail.com'
staff(5,2,'Rui do Carmo',email_desconhecido).
-staff(5,2,'Rui do Carmo','rui@gmail.com').

%Uma vacinação do utente 10 mas a única informação que sabemos do nome da vacina é que não é a 'Vacina2'
vacinacao_covid(1,10,6-6-2021, vacina_desconhecida,1).
-vacinacao_covid(1,10,6-6-2021, Vacina2,1).

%-----------------------------------------------
%CONHECIMENTO INTERDITO (valor nulo tipo 3)
%-----------------------------------------------

%utente 12 tem um número de segurança social que não pode ser conhecido
utente(12, ssocial_desconhecido, 'Vicente', 21-08-1994,'vicente@gmail.com', '965209421','Rua do Lado',[],'Estudante',1).
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :-utente(A,ssocial_desconhecido,C,D,E,F,G,H,I,J).
nulo(ssocial_desconhecido).
%Invariante que não permite a inserçãp de um número de segurança social neste utente(id=12)
+utente(A,B,C,D,E,F,G,H,I,J) :: (resposta(ssocial_desconhecido,(utente(12, ssocial_desconhecido, 'Vicente', 21-08-1994,'vicente@gmail.com', '965209421','Rua do Lado',[],'Estudante',1),nao(nulo(ssocial_desconhecido))),S), comprimento(S,N), N==0).


%membro do staff 6 tem email que ninguem pode conhecer
staff(6,1,'Clara Ribeiro', email_desconhecido).
excecao(staff(A,B,C,D)) :- staff(A,B,C,email_desconhecido).
nulo(email_desconhecido).
+staff(A,B,C,D) :: (resposta(email_desconhecido, (staff(6,1,'Clara Ribeiro', email_desconhecido),nao(nulo(email_desconhecido))),S), comprimento(S,N), N==0).


%-----------------------------------------------
%CONHECIMENTO IMPRECISO (valor nulo tipo 2)
%-----------------------------------------------

% um membro do staff pode trabalhar no centro de saude 1 ou no centro 2
excecao(staff(7,1,'Cardoso', 'patricia@gmail.com')).
excecao(staff(7,2,'Cardoso', 'patricia@gmail.com')).

% utente que tem uma profissão que pode ser Peixeira ou Talhante 
excecao(utente(13, 77777, 'Marcia Fernandes', 21-01-1998,'marcia@gmail.com', '968203012','Rua do Fundo',[],'Peixeira',1)).
excecao(utente(13, 77777, 'Marcia Fernandes', 21-01-1998,'marcia@gmail.com', '968203012','Rua do Fundo',[],'Talhante',1)).

% uma vacina que pode ter sido admistrada em dois centros distintos
excecao(vacinacao_covid(1,9,03-06-2021, 'Pfizer',1)).
excecao(vacinacao_covid(1,9,03-06-2021, 'Pfizer',2)).

% um centro que pode estar registado com moradas distintas
excecao(centro_saude(4,'Centro da Ruela', 'Rua das Ruelas', '253753029', 'ruela@gmail.com')).
excecao(centro_saude(4,'Centro da Ruela', 'Rua das Retas', '253753029', 'ruela@gmail.com')).

% utente nasceu entre 1945 e 1955 11
excecao(utente(14, 77777, 'Carolina Alves', 27-01-A,'carolinaa@gmail.com', '968520362','Rua da Amargura',[],'Reformada',1)) :- A >= 1945 , A =< 1955.

%-----------------------------------------------
%CONHECIMENTO INCERTO (valor nulo tipo 1)
%-----------------------------------------------

%-----------------------------------------------
%                   UTENTES
%-----------------------------------------------


utente(15, 75203, 'Carolina Alves', 27-01-1998,'carolina@gmail.com', telemovel_desconhecido,'Rua dos Gomes',[],'Estudante',2).


%Nome desconhecido
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,nome_desconhecido, D,E,F,G,H,I,J).

%Profissão desconhecida
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,C, D,E,F,G,H,profissao_desconhecida,J).

%Data de nascimento desconhecida
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,C,data_desconhecida,E,F,G,H,I,J).

%Número de telemóvel desconhecido
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,C,D,E,telemovel_desconhecido,G,H,I,J).

%Rua desconhecida
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,C, D,E,F,rua_desconhecida,H,I,J).

%Rua e contacto desconhecido
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,C,D,E,telemovel_desconhecido,rua_desconhecida,H,I,J).


%Centro desconhecido
excecao(utente(A,B,C,D,E,F,G,H,I,J)) :- utente(A,B,C,D,E,F,G,H,I,centro_desconhecido).

%-----------------------------------------------
%                    STAFF
%-----------------------------------------------

staff(8,centro_desconhecido,'Vasco Correia', 'vasco@gmail.com').

%Centro desconhecido
excecao(staff(A,B,C,D)) :- staff(A,centro_desconhecido,C,D).

%-----------------------------------------------
%                CENTRO DE SAUDE
%-----------------------------------------------

%Rua desconhecida
excecao(centro_saude(A,B,C,D,E)) :- centro_saude(A,B,rua_desconhecida,D,E).

%Telefone desconhecido
excecao(centro_saude(A,B,C,D,E)) :- centro_saude(A,B,C,telemovel_desconhecido,E).


%Email desconhecido
excecao(centro_saude(A,B,C,D,E)) :- centro_saude(A,B,C,D,email_desconhecido).


%Telefone e email desconhecido
excecao(centro_saude(A,B,C,D,E)) :- centro_saude(A,B,C,telemovel_desconhecido,email_desconhecido).

%-----------------------------------------------
%                VACINACAO
%-----------------------------------------------

%Centro desconhecido
excecao(vacinacao_covid(A,B,C,D,E)) :- vacinacao_covid(A,B,C,D,centro_desconhecido).

%Data desconhecida
excecao(vacinacao_covid(A,B,C,D,E)) :- vacinacao_covid(A,B,data_desconhecida,D,E).

%Nome da Vacina desconhecida
excecao(vacinacao_covid(A,B,C,D,E)) :- vacinacao_covid(A,B,C,vacina_desconhecida,E).

%----------------------------------------------------------

