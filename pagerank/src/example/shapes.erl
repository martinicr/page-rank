%%%-------------------------------------------------------------------
%%% Calculo de areas para poligonos regulares
%%% @author martinflores
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2015 8:02 PM
%%%-------------------------------------------------------------------
-module(shapes).
-author("martinflores").

% Aqui se importa la funcion sqrt del modulo math. De esta forma solamente se invoca la funcion sin tener que
% anteponer el nombre del modulo. La funcion math:pi si se tiene que referenciar de forma completa
-import(math, [sqrt/1]).

%% Dos funciones para calculo de area son exportadas. Esto se hizo porque varían en cantidad de parametro (arity)
-export([area/2]).
-export([area/4]).
-export([shape_area/1]).


% Funcion de area con dos y 4 parametros


area(square, N) ->
  N * N;
area(circle, R) ->
  math:pi() * R * R.

% Esta funcion a pesar de tener el mismo nombre, se exporta por separado
area(triangle, A, B, C) ->
  S = (A + B + C) / 2,
  sqrt(S * (S-A) * (S-B) * (S-C)).


% Funcion de area con una tupla como parametro
% Invocacion
% > shapes:shape_area(triangle, 2, 3, 4).

shape_area({square, N}) ->
  N * N;
shape_area({circle, Radius}) ->
  math:pi() * Radius * Radius;
shape_area({triangle, A, B, C}) ->
  S = (A + B + C) / 2,
  sqrt(S * (S-A) * (S-B) * (S-C));
shape_area(_Other) ->
  {error, invalid_object}.


