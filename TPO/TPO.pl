:- module(TPO,_).
:- use_package(.(meta_for)).

main:-
	display('inicio_de_la_iteracion'),nl,
	for I = 2 to 6 do ( M is I * 2, write(M) , nl),
	display('fin').

main2:-
    display('inicio_de_la_iteracion'),nl,
    for I = 2 to 6 do ( M is I * 2,for T=1 to 3 do(L is I + T,display('L is '),display(L),nl),
                        display('M is '), display(M) , nl),
    display('fin').


