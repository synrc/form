-module(forms_stand).
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

form(?MODULE)            -> [];
form(Name=_)             -> Name:new(Name).

render({ok,_,R}) -> R;
render({error,Name}) -> #h1{body=[nitro:to_list(Name)," failed"]}.

main() -> [].
body() -> [].

event(init) ->
    Forms = [ begin
        Name = forms:atom([filename:basename(F,".erl")]),
        Class = nitro:to_list(case nitro:qc(<<"class">>) of undefined -> Name; X -> X end),
        case string:str(nitro:to_list(Name),Class) == 1 orelse nitro:qc(<<"class">>) == <<>> of
              true ->
        Render = try {ok,Name,form(Name)}
                       catch E:R
                          -> n2o:info(?MODULE,"~w~n",[n2o:stack(E,R)]),
                             {error,Name} end,
        Form = #panel{class=column1,
                 style="margin: -10 0 0 0;"
                    ++ case Render of
                            {ok,_,_} -> [];
                             _ -> "color: red;" end,
                 body= [ #h3{body=nitro:to_list(Name)}, render(Render)] },
        self() ! {direct,{form, Form}},
        Render;
             _ -> skip end
    end || F <- lists:sort(mad_repl:wildcards(["src/forms/**/*.erl"])) ],
    self() ! {direct,{info,Forms}},
    n2o:info(?MODULE,"Init: ~p~n",[self()]);

event({form,Form}) -> nitro:insert_bottom(stand,Form);
event({info,Forms}) ->
    {I,Broken} = lists:partition(fun({ok,_,_}) -> true; (_) -> false end,Forms),
    Installed = lists:map(fun({ok,N,_}) -> N end,I),
    n2o:info(?MODULE,"Overal forms: ~p~n",[length(Forms)]),
    case length(Installed) == length(Forms) of
         true ->  n2o:info(?MODULE,"PASSED ~p: ~p~n",[length(Installed),Installed]),
                  nitro:wire(lists:concat(["console.log('PASSED ",length(Installed),"');"]));
         false -> n2o:info(?MODULE,"OK ~p: ~p~n",    [length(Installed),Installed]),
                  nitro:wire(lists:concat(["console.log('OK ",length(Installed),"');"])),
                  n2o:info(?MODULE,"ERROR ~p: ~p~n", [length(Broken),   Broken]),
                  nitro:wire(lists:concat(["console.log('ERROR ",length(Broken),"');"])) end;
event(Event) ->   n2o:info(?MODULE,"unknown: ~p",    [Event]).
