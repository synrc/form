-module(forms_routes).
-copyright("N2O Community").
-include_lib("n2o/include/n2o.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, #cx{req=Req}=Cx) ->
    #{path:=Path}=Req,
    Fix  = route_prefix(Path),
    n2o:info(?MODULE,"Route: ~p~n",[{Fix,Path}]),
    {ok, State, Cx#cx{path=Path,module=Fix}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>)                     -> forms_index;
route(<<"index",_/binary>>    ) -> forms_index;
route(<<"login",_/binary>>)     -> forms_login;
route(<<"app/index",_/binary>>) -> forms_index;
route(<<"app/login",_/binary>>) -> forms_login;
route(_) -> forms_index.
