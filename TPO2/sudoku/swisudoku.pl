:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).

:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(www_browser)).

:- use_module(library(clpfd)).

:- dynamic sudoku/1.

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(files, '/f', []).
:- http_handler(files(.), serve_files, [prefix]).
serve_files(Request) :-
	 http_reply_from_files('static', [], Request).
serve_files(Request) :-
	  http_404([], Request).

sudoku_blank([[_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_]]).


solvesudoku(Rows) :-
        length(Rows, 9), maplist(same_length(Rows), Rows),
        append(Rows, Vs), Vs ins 1..9,
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C), blocks(D, E, F), blocks(G, H, I),
        labeling([],Vs).

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_distinct([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).



:- http_handler(root(sudoku),sudokupage,[]).
:- http_handler(root(solvesudoku),solvesudokupage,[]).
:- http_handler(root(loadsudoku), loadsudokupage,[]).
:- http_handler(root(cleansudoku), cleansudokupage,[]).


cleansudokupage(_Request):-
    sudoku_blank(Rows),
    retract(sudoku(_)),
    asserta(sudoku(Rows)),
    http_redirect(see_other,'/sudoku',_).    


unifysudoku(end_of_file,_,_) :- !.
unifysudoku(Term,input,Rows):-
    unifys(Term,Rows),
    read(input,Term1),
    unifysudoku(Term1,input,Rows).

unifys(Var=Val,Rows):-
    number_string(N,Val),
    atom_concat('v',M,Var),
    atom_codes(M,[XR,YR]),
    X is XR - 48,
    Y is YR - 48,
    nth1(X,Rows,List),
    nth1(Y,List,V),
    V = N.

   


getparam(ParamList,Rows):-
    getparam(1,Rows,ParamList,[]).

getparam(10,[]) --> [].
getparam(N,[R|Rows]) -->
    {N1 is N + 1},
    getparam(N,1,R),
    getparam(N1,Rows).

getparam(_,10,[]) --> [].
getparam(N,M,[V|R]) -->
    { atomic_list_concat(['v',N,M],A),
      functor(F,A,2),arg(1,F,V),arg(2,F,[optional(true),integer]),
      M1 is M + 1 },
    [F],
    getparam(N,M1,R).

loadsudokupage(Request) :-
    http_parameters(Request,[file(File,[default("")])]),
    ( File == "" -> true ;
      (
          open(File, read, _Fd, [alias(input)]),
          sudoku_blank(Rows),
          read(input,Term),
          unifysudoku(Term,input,Rows),
          close(input),
          retract(sudoku(_)),
          asserta(sudoku(Rows))
      )),
    http_redirect(see_other,'/sudoku',_).    

solvesudokupage(Request) :-
    sudoku_blank(Rows),
    getparam(ParamList,Rows),
    http_parameters(Request,ParamList),
    solvesudoku(Rows),
    retract(sudoku(_)),
    asserta(sudoku(Rows)),
    http_redirect(see_other,'/sudoku',_).    



sudokupage(_Request):-
    sudoku(Rows),
    reply_html_page([
	\['<meta name="viewport" content="width=device-width, initial-scale=1.0"s'],
        \['<link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">'],

        \['<link rel="stylesheet" href="https://www.w3schools.com/w3css/4/w3.css">'],
              title('sudoku')
    ],[
        header([class="w3-container  w3-light-grey"],[
            \html_requires(files('sudokutable.css')),
            h1([class="w3-jumbo"],"Sudoku"),
            div(class="w3-container w3-grey w3-center w3-row-padding",
            \rendersudoku(Rows))
            ])
    ]).
    

rendersudoku(Rows) -->
    html([
        div(class="w3-half",[
        form([action="/solvesudoku",method="post"],[

             table(
                             tbody(
                                          \sudokurows(Rows,1)
                             )),
             button([class="w3-button w3-green",type="submit"],"Resolver")

                 ]
                 
            ),
            form([action="/cleansudoku",method="post"],[
                     button([class="w3-button w3-green",type="submit"]," Limpiar ")
                 ]
                )
            ]),
        div(class="w3-quarter",[
            form([action="/loadsudoku",method="post"],[
                     input([class="w3-input w3-green",type="file",name="file"],[]),
                     button([class="w3-button w3-green",type="submit"],"Cargar")
                 ]
                )

            ]
            )
        
            ]).


sudokurows([],_) --> [].
sudokurows([R|Rows],N) -->
    {N1 is N + 1},
                html([
                            tr(
                                      \sudrow(R,N,1)
                            ),
                            \sudokurows(Rows,N1)
                        ]).



sudrow([],_,_) --> [].
sudrow([V|R],N,M) -->
    { (nonvar(V) -> atom_number(A,V) ; A = ''),
      M1 is M + 1
    },  
     html([
                 td(
                      input([type="text",value=A,name="v"+N+M],[])
                 ),

                 \sudrow(R,N,M1)
                 ]).
                          
                                      
                
    
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- server(5000).
%% :- asserta(sudoku([[_,_,_, _,_,_, _,_,_],
%%                    [_,_,_, _,_,3, _,8,5],
%%                    [_,_,1, _,2,_, _,_,_],
                   
%%                    [_,_,_, 5,_,7, _,_,_],
%%                    [_,_,4, _,_,_, 1,_,_],
%%                    [_,9,_, _,_,_, _,_,_],

%%                    [5,_,_, _,_,_, _,7,3],
%%                    [_,_,2, _,1,_, _,_,_],
%%                    [_,_,_, _,4,_, _,_,9]])).

:- asserta(sudoku([[_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_]])).

:- www_open_url("http:/localhost:5000/sudoku").
    
