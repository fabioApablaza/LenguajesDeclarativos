:-package(meta_for).% Se define el paquete

%Modulo donde se ubica el predicado que se ba a ejecutar en tiempo de ejecucion
:-use_module(meta_for_runtime,[do/4]).
%Se carga el codigo definido en meta_for_trans durante el tiempo de compilacion
:- load_compilation_module(meta_for_trans).
%Se define que el predicado traduccion/2 realizara la traduccion de las clausulas
:- add_sentence_trans(meta_for_trans:traduccion/2,700).