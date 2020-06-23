:- module(ciaosudoku,_,[pillow,dcg,functional]).

:- use_module(library(service/service_registry), [reload_service_registry/0]).
:- use_module(library(service/serve_http), []). % (implements httpserv.handle/3)
:- include(library(http/http_server_hooks)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_forms)).

:- use_module(library(format)).
:- use_module(library(system)).
:- use_module(library(lists)).
:- use_module(engine(stream_basic)).
:- use_module(library(read)).

:- use_package(clpfd).
:- use_package(datafacts).

:- data(sudoku/1).


solve(T):-
	trans_m(T,Tt),
	all_vars(T,V),
        T = [A,B,C,D,E,F,G,H,I],
        blocks(A, B, C), blocks(D, E, F), blocks(G, H, I),
        
        every_list_diff(T),
        every_list_diff(Tt),

        labeling([],V),
	!. % just one solution

blocks([], [], []).
blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
        all_different([A,B,C,D,E,F,G,H,I]),
        blocks(Bs1, Bs2, Bs3).


trans_m([[]|_],[]).
trans_m(M,[C1|Cn]):- trans_v(M,C1,R), trans_m(R,Cn).

trans_v([],[],[]).
trans_v([[C11|C1n]|C],[C11|X],[C1n|Y]):- trans_v(C,X,Y).

every_list_diff([]).
every_list_diff([L|Ls]):-
        all_different(L),
        every_list_diff(Ls).

all_vars([], []).
all_vars([N|R], V):- nonvar(N), N = [], !, all_vars(R, V).
all_vars([[X|Xs]|Rs], [X|Ys]):-
        X in 1..9,
        all_vars([Xs|Rs], Ys).



sudoku_blank([[_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_],
              [_,_,_, _,_,_, _,_,_]]).

limpiarsudoku :-
    sudoku_blank(R),             
    set_fact(sudoku(R)).



 resolversudoku(Input):-
     sudoku_blank(R),
     instanciarsudoku(1,R,Input),
     solve(R),
     set_fact(sudoku(R)).

instanciarsudoku(10,[],_).
instanciarsudoku(N,[C|R],Input):-
    instanciarfila(N,1,C,Input),
    N1 is N + 1,
    instanciarsudoku(N1,R,Input).

instanciarfila(_,10,[],_).
instanciarfila(N,M,[V|C],Input):-
    atom_number(AN,N),
    atom_number(AM,M),
    atom_concat('v',AN,A1),
    atom_concat(A1,AM,VNM),
    get_form_value(Input,VNM,Atomo),
     (form_empty_value(Atomo) -> true
     ;
                   V = Atomo
      ),
    M1 is M + 1,
    instanciarfila(N,M1,C,Input).

 cargarsudoku(File):-
     (  form_empty_value(File) -> true
     ;
         open(File,read,S),
         sudoku_blank(Rows),
         read(S,Term),
         unifysudoku(Term,S,Rows),
         close(S),
         set_fact(sudoku(Rows))
         ).

unifysudoku(end_of_file,_,_) :- !.
unifysudoku(Term,S,Rows):-
    unifys(Term,Rows),
    read(S,Term1),
    unifysudoku(Term1,S,Rows).

unifys(Var=[Val],Rows):-
     N is Val - 48,
     atom_concat('v',M,Var),
     atom_codes(M,[XR,YR]),
     X is XR - 48,
     Y is YR - 48,
    nth(X,Rows,List),
    nth(Y,List,V),
    V = N.



'httpserv.handle'("/sudoku",Request,Response) :-
    http_parse_form(Request,Input),
    get_form_value_atm(Input,accion,Accion),
    ( Accion == 'limpiar' ->  limpiarsudoku  ; true),
    ( Accion == 'cargar' ->  get_form_value(Input,file,File),cargarsudoku(File)  ; true),
    ( Accion == 'solve' ->  resolversudoku(Input)  ; true),
        sudoku(Rows),
    HTML = [
        start,
        begin(head),
          "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">",
          "<link href=\"https://www.w3schools.com/w3css/4/w3.css\" rel=\"stylesheet\">",
          title('sudoku'),
          begin(style,[type='text/css']),
"          * { box-sizing: border-box; }",
"table { margin: 10px; }",
"tr:first-child td {",
"	border-top-color: black;",
"}",
"tr:nth-child(3n) td {",
"	border-bottom-color: black;",
"}",
"td {",
"	border: 1px solid lightgrey;",
"	height: 40px;",
"	width: 40px;",
"}",
"td:first-child {",
"	border-left-color: black;",
"}",
"td:nth-child(3n) {",
"	border-right-color: black;",
"}",
"input {",
"	padding: 0;",
"	text-align: center;",
"	border: 0;",
"	height: 40px;",
"	width: 40px;",
"	text-align: center;",
"}",
"",
"input:hover {",
"	background: #eee;",
"}",
          end(style),
        end(head),
        begin(body),
        header([class="w3-container  w3-light-grey"],[
            h1([class="w3-jumbo"],"Sudoku"),
            div([class="w3-container w3-grey w3-center w3-row-padding"],[
                div([class="w3-half"],[
                    start_form("/sudoku",[method="post"]), 
                        table(
                            tbody(
                               FILAS 
                            )),
                        input(hidden,[value="solve",name="accion"]),
                        button([class="w3-button w3-green",type="submit"],"Resolver"),
                    end_form,
                    start_form("/sudoku",[method="post"]),
                    input(hidden,[value="limpiar",name="accion"]),
                     button([class="w3-button w3-green",type="submit"]," Limpiar "),
                   end_form
                ]),

                div([class="w3-quarter"],[
                    start_form("/sudoku",[method="post"]),
                        input(hidden,[value="cargar",name="accion"]),
                        input(file,[class="w3-input w3-green",name="file"]),
                        button([class="w3-button w3-green",type="submit"],"Cargar"),
                    end_form
                    ]
                   )
                
                ]

               )
            ]),

        end(body),
        end],
        sudokurows(Rows,1,FILAS,[]),
        html2terms(Str, HTML),
        Response = html_string(Str).

sudokurows([],_) --> [].
sudokurows([R|Rows],N) -->
    {N1 is N + 1,
     sudrow(R,N,1,COLS,[]) 
    },
    [tr(COLS)],
    sudokurows(Rows,N1).

sudrow([],_,_) --> [].
sudrow([V|R],N,M) -->
    { (nonvar(V) -> atom_number(A,V) ; A = ''),
      M1 is M + 1
    },
    [
        td(
            input(text,[value=A,name="v" ++ ~number_codes(N) ++ ~number_codes(M)])
        )
    ],
    sudrow(R,N,M1).



    
main :-
    limpiarsudoku,
    Port = 8000,
    http_bind(Port),
    format(user_error, "=> starting HTTP server~n", []),
    reload_service_registry,
    format(user_error, "   Server reachable at http://localhost:~w~n", [Port]),
   % shell('firefox -new-tab http://localhost:8000/sudoku'),
    http_loop(ExitCode),
    format(user_error, "=> HTTP server finished with exit code ~w~n", [ExitCode]),
    halt(ExitCode).

:- initialization(main).
