:-module(pagwebTPO2,_,[pillow]).
/*
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
*/
:- use_package(clpfd).
:- use_package(datafacts).
:- use_package(pillow).

%:-use_module(library(http/thread_httpd)).
%:- http_handler('/phone', phone, []).