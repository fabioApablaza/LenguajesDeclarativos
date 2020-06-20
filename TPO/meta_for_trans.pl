:- module(meta_for_trans,_).
:-use_module(library(lists)).
%Definicion de los operadores
:-op(700,fy,for).
:-op(600,xfx,to).
:-op(500,xfx,do).
%Inicio de la traduccion
traduccion(0,0).
traduccion(end_of_file,end_of_file).
traduccion((:- A),(:- A)).
traduccion((A :- B),(A :- NB)):-
    sequence_to_list(B,L), % Se transforma la sentencia en lista para poder trabajar con ella.
    buscar_for(L,N),
    sequence_to_list(NB,N). % El resultado se vuelve a transformar en sentencia
buscar_for([],[]). %Predicado utilizado para buscar las clausulas for
buscar_for([(for I = A to X do(M))|L],[do(I,A,X,M3)|T]):-
    sequence_to_list(M,M2),
    buscar_for(M2,M3),
    buscar_for(L,T).
buscar_for([H1|T1],[H1|T2]):-
    buscar_for(T1,T2).
    
    
    