:- module(meta_for_runtime,do/4).
%Definicion del predicado que se va a ejecutar en tiempo de ejecucion
do(I,A,X,[H|T]):-            % I es el parametro I del for, A es el valor del parametro I
    	between(A,X,I),  % X es el valor por el cual el bucle debe dejar de repetirse
        llamar(I,[H|T]),     % Llamada al predicado llamar() para ejecutar los predicados dentro del bucle
	fail.
do(_,_,_,_).% Condicion veradera del predicado
llamar(_,[]).% Predicado para llamar a los predicados dentro del bucle
llamar(I,[H|T]):-
    call(H),
    llamar(I,T).