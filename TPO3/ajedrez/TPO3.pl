:-module(TPO3,_,[pillow,clpfd,dcg]).%< y >
%:-module(TPO3,_).
%:-use_package(library(pillow)).
%:-use_package(library(clpfd)).
%:-use_package(library(dcg)).
%:-use_package(functional).

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

:- use_package(datafacts).
:- data(ajedrez/1).
%Predicado para obetener un tablero de ajedrez con los valores de los casilleros vacios
tablero_vacio(L):-
    L=[L1,L2,L3,L4,L5,L6,L7,L8],
    L1=[_,_,_,_,_,_,_,_],
    L2=[_,_,_,_,_,_,_,_],
    L3=[_,_,_,_,_,_,_,_],
    L4=[_,_,_,_,_,_,_,_],
    L5=[_,_,_,_,_,_,_,_],
    L6=[_,_,_,_,_,_,_,_],
    L7=[_,_,_,_,_,_,_,_],
    L8=[_,_,_,_,_,_,_,_].
    
vaciar_tablero:-
    tablero_vacio(P),
    set_fact(ajedrez(P)).
%Predicado principal para mostrar la unica solucion del problema
resolver_ajedrez(Input):-
    tablero_vacio(P),
    instanciar_ajedrez(1,P,Input,R),
    resolver(P,R,P1),
    set_fact(ajedrez(P1)).

         /*ACA ESTA EL PROBLEMA*/   
/*instanciar_ajedrez(9,[],_,[]).
instanciar_ajedrez(N,[C|R],Input,S):-
    instanciar_fila(N,1,C,Input,A),
    N1 is N + 1,
    instanciar_ajedrez(N1,R,Input,T),
    append(A,T,S).

instanciar_fila(_,9,[],_,[]).
instanciar_fila(N,M,[V|C],Input,[((M,N),V)|T]):-
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
    instanciar_fila(N,M1,C,Input,T).
*/
instanciar_ajedrez(9,[],_,[]).
instanciar_ajedrez(N,[C|R],Input,S):-
    instanciar_fila(N,1,C,Input,A),
    N1 is N + 1,
    instanciar_ajedrez(N1,R,Input,T),
    %eliminar_coordenadas_inutiles(A,A1),
    append(A,T,S).

/*eliminar_coordenadas_inutiles([],[]).
eliminar_coordenadas_inutiles([((N,M),V)|T],[((N,M),V)|T1]):-
    V >0,
    eliminar_coordenadas_inutiles(T,T1).
eliminar_coordenadas_inutiles([((_,_),'_')|T],T1):-
    eliminar_coordenadas_inutiles(T,T1).*/

instanciar_fila(_,9,[],_,[]).

instanciar_fila(N,M,[V|C],Input,[((M,N),V)|T]):-
    atom_number(AN,N),
    atom_number(AM,M),
    atom_concat('v',AM,A1),
    atom_concat(A1,AN,VNM),
    get_form_value(Input,VNM,Atomo),
     (form_empty_value(Atomo) -> true
     ;
            V = Atomo
      ),
    M1 is M + 1,
    instanciar_fila(N,M1,C,Input,T).

%Predicado para cargar las posiciones en el tablero con un archivo
cargar_tablero(File):-
     (  form_empty_value(File) -> true
     ;
         open(File,read,S),
         tablero_vacio(Rows),
         read(S,Term),
         unificar_ajedrez(Term,S,Rows),
         close(S),
         set_fact(ajedrez(Rows))
         ).

unificar_ajedrez(end_of_file,_,_) :- !.
unificar_ajedrez(Term,S,Rows):-
    unificar(Term,Rows),
    read(S,Term1),
    unificar_ajedrez(Term1,S,Rows).

unificar(Var=[Val],Rows):-
     N is Val - 48,
     atom_concat('v',M,Var),
     atom_codes(M,[XR,YR]),
     X is XR - 48,
     Y is YR - 48,
    nth(X,Rows,List),
    nth(Y,List,V),
    V = N.

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
resolver(P,R,P1):-
    L=[TC,TF,CC,CF,AC,AF,RC,RF],
    domain(L,1,8),
%    (all_different([TC,CC,AC,RC]);
%        all_different([TF,CF,AF,RF])),
        recorrer_Tablero(R,L),
    label(L),
    L2=[((TC,TF),5),((CC,CF),6),((AC,AF),7),((RC,RF),8)],
    instanciar(P,L2,P1).

%Torre: &#9820; Caballo: &#9822; Alfil: &#9821; Reina: &#9819;

%Predicado para recorrer la lista de los casilleros y determinar las posiciones de las piezas

recorrer_Tablero([],_).
recorrer_Tablero([((X,Y),N)|T],[TC,TF,CC,CF,AC,AF,RC,RF]):-
    (pos_torre(TF,TC,(X,Y),N1),
    pos_caballo(CF,CC,(X,Y),N2),
    pos_alfil(AF,AC,(X,Y),N3),
    pos_reina(RF,RC,(X,Y),N4)),
    N1+N2+N3+N4#=N,
    recorrer_Tablero(T,[TC,TF,CC,CF,AC,AF,RC,RF]).

instanciar(T,[],T).
instanciar([H|T],[((C,F),V)|T2],T4):-
    nth(F,[H|T],L),
    %element_at(V,C,L,L1),
    %element_at(L1,F,[H|T],T3),
    reemplazar_elemento(V,C,L,L1),
    reemplazar_elemento(L1,F,[H|T],T3),
    instanciar(T3,T2,T4).

reemplazar_elemento(E,1,[_|T],[E|T]).
reemplazar_elemento(E,N,[H|T],[H|T1]):-
    M is N-1,
    reemplazar_elemento(E,M,T,T1).
%element_at(E,1,[_|Ls],[E|Ls]).
%element_at(E,X,[L|Ls0],[L|Ls]):-
%    X#>1,
%    X#= X0 +1,
%    element_at(E,X0,Ls0,Ls).

'httpserv.handle'("/ajedrez",Request,Response) :-
    http_parse_form(Request,Input),
    get_form_value_atm(Input,accion,Accion),
    ( Accion == 'vaciar' ->  vaciar_tablero  ; true),
    ( Accion == 'cargar' ->  get_form_value(Input,file,File),cargar_tablero(File)  ; true),
    ( Accion == 'resolver' ->  resolver_ajedrez(Input)  ; true),
    ajedrez(L),
    HTML=[start,
          begin(head),
          title('TPO3 Ajedrez'),
          begin(style,[type='text/css']),
          " body { background-color: #1a1a1a;}",
          " header{background-color: #009933;
                    text-align:center;}",
          "#principal{background-color:#009933;
                      margin:40px;
                      padding:60px;
                     }",
          "  * { box-sizing: border-box; }",
          "table { margin: 10px;
                   background-color: white;
            }",
          "tr:nth-child(even) td:nth-child(odd) input{
          	background:black;
                color:white;
          }",
          "tr:nth-child(odd) td:nth-child(even) input{
          	background:black;
                color:white;
          }",
          ".divbottom{
           display:flex;
           align-items:center;
          }",
          "td{
	          height: 40px;
	          width: 40px;
                  background-color: black;
               }",
          "input{ padding:0; 
                  text-align:center;
                  border:0;
                  height:40px;
                  width:40px;
                 }",
          end(style),
          end(begin),
          begin(body),
          begin(header),
          h1("TPO3 Ajedrez"),
          end(header),
          div([id='principal'],[
              start_form("/ajedrez",[method="post"]),
              table(
                  tbody(Lista)
              ),
              input(hidden,[value="resolver",name="accion"]),
              button([class="w3-button w3-black w3-round-xxlarge",type="submit"],"Resolver"),
              end_form,
              start_form("/ajedrez",[method="post"]),
                    input(hidden,[value="vaciar",name="accion"]),
                     button([class="w3-button w3-black w3-green",type="submit"]," Vaciar "),
                     end_form,
                     div([class="w3-half divbottom"],[
                    start_form("/ajedrez",[method="post"]),
                        input(hidden,[value="cargar",name="accion"]),
                        input(file,[class="w3-input w3-green",name="file"]),
                        button([class="w3-button w3-green button4",type="submit"],"Cargar"),
                    end_form
                    ]
                   )

          ]),
          end(body),
          end],
    tableroajedrez(L,1,Lista,[]),
    html2terms(Str, HTML),
    Response = html_string(Str).


tableroajedrez([],_) --> [].
tableroajedrez([R|Filas],N) -->
    {N1 is N + 1,
     tablerofilas(R,N,1,COLS,[]) 
    },
    [tr(COLS)],
    tableroajedrez(Filas,N1).

tablerofilas([],_,_) --> [].
tablerofilas([V|R],N,M) -->
    { ((V==5,A='&#9820;'); %Si el valor contenido en V es 5 entonces es una torre
        (V==6,A='&#9822;');%Si el valor contenido en V es 6 entonces es un caballo
        (V==7,A='&#9821;');%Si el valor contenido en V es 7 entonces es un alfil
        (V==8,A='&#9819;');%Si el valor contenido en V es 8 entonces es una reina
        (V==0,A='');%Si el valor contenido en V es 0 entonces no se mostrara ningun valor
        (nonvar(V) -> atom_number(A,V); A = '')),
      M1 is M + 1,
    number_codes(N,AN),
    number_codes(M,AM),
    append("v",AM,A1),
    append(A1,AN,VNM)
    },
    [
        td(
            input(text,[value=A,name=VNM /*  "v"   ++ ~number_codes(N) ++ ~number_codes(M)*/])
        )
    ],
    tablerofilas(R,N,M1).

main:-
    vaciar_tablero,
    Port = 8000,
    http_bind(Port),
    format(user_error, "=> starting HTTP server~n", []),
    reload_service_registry,
    format(user_error, "   Server reachable at http://localhost:~w~n", [Port]),
    %shell('firefox -new-tab http://localhost:8000/ajedrez'),
    http_loop(ExitCode),
    format(user_error, "=> HTTP server finished with exit code ~w~n", [ExitCode]),
    halt(ExitCode).

%:- initialization(main).

/*resolver([[_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_]],[((5,7),4),((5,5),3),((4,8),2),((1,7),2),((1,5),2),((7,5),2),((2,4),2),((4,4),2),((1,3),2),((3,3),2),((7,3),2),((5,1),2)],R). 
Segunda configuracion
resolver([[_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_]],[((8,3),2),((3,4),3),((5,4),2),((2,5),4),((4,5),2),((6,5),3),((2,7),2),((8,7),2),((3,8),2)],R,L). 

resolver([[_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_],
    [_,_,_,_,_,_,_,_]],[((4,1),2),((1,2),2),((5,2),4),((1,4),2),((5,4),3),((7,4),2),((2,5),2),((4,5),2),((1,6),2),((3,6),2),((7,6),2),((5,8),2)],R).

Resolucion primero configuracion
[[_,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_],
 [_,_,C,_,_,_,_,_],
 [_,_,R,_,_,_,_,_],
 [_,_,_,_,_,_,_,A],
 [_,_,_,_,T,_,_,_],
 [_,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_]]

[[_,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_],
 [_,_,_,R,_,_,_,2],
 [_,_,3,_,2,_,_,_],
 [_,4,_,2,_,3,_,T],
 [A,_,_,C,_,_,_,_],
 [_,2,_,_,_,_,_,2],
 [_,_,2,_,_,_,_,_]]

[[_,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_],
 [8,_,_,_,_,_,_,_],
 [_,_,_,_,6,_,_,_],
 [7,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_],
 [_,_,_,_,_,_,_,_]]
*/