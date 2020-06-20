:-module(TPO2,_).%< y >
:- use_module(library(hiordlib)).
:- use_package(clpfd).
:- use_module(library(llists)).
:- use_module(library(lists)).
%:- use_module(library(clpfd)).
test(X, Y):-
    X in 0..10,
    Y in 4..8,
    X #> Y.
increase([]).
increase([H|T]):-
    H in 1..10,
    domain(T,1,10),    
    menor(H,T),
    increase(T).
menor(_,[]).
menor(H1,[H|_]):-
    H1 #< H.
puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-   
        Vars = [S,E,N,D,M,O,R,Y],     
        Vars in 0..9,      
        all_different(Vars),        
                  S*1000 + E*100 + N*10 + D +     
                  M*1000 + O*100 + R*10 + E #=
        M*10000 + O*1000 + N*100 + E*10 + Y,
        M #\= 0, S #\= 0,    
%		label(Vars).  

%9 ?- puzzle(X).
%X = ([9, 5, 6, 7]+[1, 0, 8, 5]=[1, 0, 6, 5, 2]) ;  
%false.
%
%sudoku(Rows) :-
%        length(Rows, 9), maplist(same_length(Rows), Rows),
%        append(Rows, Vs), Vs in 1..9,
%        maplist(all_different, Rows),
%        transpose(Rows, Columns),
%        maplist(all_different, Columns),
%        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
%        blocks(As, Bs, Cs),
%        blocks(Ds, Es, Fs),
%        blocks(Gs, Hs, Is).

blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_different([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

solution(Ys):-
    Ys =[_,_,_,_,_,_,_,_],
    domain(Ys,1,8),
    all_different(Ys),
    safe(Ys),
    labeling([],Ys).

safe([]).
safe([Y|Ys]):-
    no_attack(Y, Ys, 1),
    safe(Ys).

no_attack(_,[],_).
no_attack(Y1,[Y2|Ys],D):-
    D #\= Y1-Y2,
    D #\= Y2-Y1,
    D1 is D +1,
    no_attack(Y1, Ys, D1).
