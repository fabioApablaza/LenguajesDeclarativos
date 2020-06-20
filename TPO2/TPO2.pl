:-module(TPO2,_).
:-use_package(clpfd).%< y >
:- use_module(library(lists)).
:- use_module(library(hiordlib)).
%Nombre: Apablaza Fabio
%Legajo: FAI 2039
% [_,_,_,2,_,_,_,_]
% [2,_,_,_,4,_,_,_]
% [_,_,C,_,_,_,_,_]
% [2,_,R,_,3,_,2,_]
% [_,2,_,2,_,_,_,A]
% [2,_,2,_,T,_,2,_]
% [_,_,_,_,_,_,_,_]
% [_,_,_,_,2,_,_,_]

/*

|-|-|-|2|-|-|-|-|
|2|-|-|-|4|-|-|-|
|-|-|C|-|-|-|-|-|
|2|-|R|-|3|-|2|-|
|-|2|-|2|-|-|-|A|
|2|-|2|-|T|-|2|-|
|-|-|-|-|-|-|-|-|
|-|-|-|-|2|-|-|-|


*/
%Predicado principal para mostrar la unica solucion del problema
main:-
    P=[TC,TF,CC,CF,AC,AF,RC,RF],
    resolver(P),
    display('Solucion:'),
    nl,
    display('Torre: '),
    display('('),
    display(TC),
    display(','),
    display(TF),
    display(')'),
    nl,
    display('Caballo: '),
    display('('),
    display(CC),
    display(','),
    display(CF),
    display(')'),
    nl,
    display('Alfil: '),
    display('('),
    display(AC),
    display(','),
    display(AF),
    display(')'),
    nl,
    display('Reina: '),
    display('('),
    display(RC),
    display(','),
    display(RF),
    display(')').

%predicado para obtener la posicion de la pieza torre
pos_torre(_,_,(_,_),0).
pos_torre(F,C,(X,Y),1):-
    ((F #= Y,C #\=X);(F #\=Y,C #= X)).

%Predicado para obtener la posicion de la pieza caballo
pos_caballo(_,_,(_,_),0).
pos_caballo(F,C,(X,Y),1):- 
    ((F #=Y+1;F #= Y-1),(C#= X+2; C#= X-2));
     ((F #=Y+2;F #= Y-2),(C#= X+1; C#= X-1)).

%Predicado para obtener la posicion de la pieza alfil
pos_alfil(_,_,(_,_),0).
pos_alfil(F,C,(X,Y),1):-
    M in 1..8,
    ((C#=X-M,(F#=Y+M;F#=Y-M));
     (C#=X+M,(F#=Y+M;F#=Y-M))).

%Predicado para obtener la posicion de la pieza reina
pos_reina(_,_,(_,_),0).
pos_reina(F,C,(X,Y),1):-
    M in 1..8,
    ((F #= Y,C #\=X);
     (F #\=Y,C #= X);
     (C#=X-M,(F#=Y+M;F#=Y-M));
     (C#=X+M,(F#=Y+M;F#=Y-M))).

%Predicado donde se definen los casilleros a atacar y se definen las posiciones de las piezas
resolver(P):-
    domain(P,1,8),
    recorrer_Tablero([%Se ingresan los casilleros que se desean atacar
                      ((5,7),4),
                      ((5,5),3),
                      ((4,8),2),
                      ((1,7),2),
                      ((1,5),2),
                      ((7,5),2),
                      ((2,4),2),
                      ((4,4),2),
                      ((1,3),2),
                      ((3,3),2),
                      ((7,3),2),
                      ((5,1),2)
                     ],P),
    label(P).
%Predicado para recorrer la lista de los casilleros y determinar las posiciones de las piezas
recorrer_Tablero([],_).
recorrer_Tablero([((X,Y),N)|T],[TC,TF,CC,CF,AC,AF,RC,RF]):-
    (pos_torre(TF,TC,(X,Y),N1),
    pos_caballo(CF,CC,(X,Y),N2),
    pos_alfil(AF,AC,(X,Y),N3),
    pos_reina(RF,RC,(X,Y),N4)),
    N1+N2+N3+N4#=N,
    recorrer_Tablero(T,[TC,TF,CC,CF,AC,AF,RC,RF]).