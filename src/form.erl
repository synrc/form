-module(form).

-behaviour(supervisor).

-behaviour(application).

-include_lib("form/include/meta.hrl").

-compile(export_all).

-export([start/0,
         start/1,
         start/2,
         stop/1,
         new/2,
         new/3,
         init/1,
         id/0,
         dispatch/2]).

-export([steps/3, caption/3, fields/3, buttons/3]).

-export([field/3, field/1]).

-define(M,
        application:get_env(form, module, form_backend)).

% FORM API

sources(Object) -> sources(Object, []).

sources(Object, Options) ->
    (?M):sources(Object, Options).

type(Object) -> (?M):type(Object).

kind(Options) -> (?M):kind(Options).

pos(Object, X) -> (?M):pos(Object, X).

extract(Object, X, Opt) -> (?M):extract(Object, X, Opt).

extract(Object, X, Ref, Opt) ->
    (?M):extract(Object, X, Ref, Opt).

evoke(Object, X, Value) -> (?M):evoke(Object, X, Value).

id() -> fun (X) -> X end.

atom(List) when is_list(List) ->
    string:join([nitro:to_list(L) || L <- List], "_");
atom(Scalar) -> nitro:to_list(Scalar).

dispatch(Object, Options) ->
    (?M):dispatch(Object, Options).

new(A, B) -> new(A, B, []).

new(Doc = #document{}, Object, Opt) ->
    (?M):new(Doc, Object, Opt);
new(Document, _O, _) -> Document.

steps(Doc, Obj, Opt) -> (?M):steps(Doc, Obj, Opt).

caption(Doc, Obj, Opt) -> (?M):caption(Doc, Obj, Opt).

fields(Doc, Obj, Opt) -> (?M):fields(Doc, Obj, Opt).

field(Field, Obj, Opt) -> (?M):field(Field, Obj, Opt).

field(Field) -> (?M):field(Field).

buttons(Doc, Obj, Opt) -> (?M):buttons(Doc, Obj, Opt).

translate(A, B) -> (?M):translate(A, B).

translate(A) -> (?M):translate(A).

val(A, B) -> (?M):val(A, B).

fieldType(A, B, C, D, E) ->
    (?M):fieldType(A, B, C, D, E).

fieldType(A, B, C, D) -> (?M):fieldType(A, B, C, D).

proto(A) -> (?M):proto(A).

update(A) -> (?M):update(A).

% OTP

stop(_) -> ok.

start() -> start(normal, []).

start(_) -> start().

init([]) -> {ok, {{one_for_one, 5, 10}, []}}.

start(_, _) ->
    supervisor:start_link({local, form}, form, []).
