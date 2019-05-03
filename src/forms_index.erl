-module(forms_index).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

main() -> [].
body() -> [].

event({client,{form,F}}) ->

      Module = nitro:to_atom(filename:basename(F,".erl")),
      FORM   = Module:new(Module,Module:id()),

      nitro:insert_bottom(stand, #h3{body=nitro:to_binary(Module)}),
      nitro:insert_bottom(stand, forms:new(FORM, [])),
      nitro:insert_bottom(stand, nitro:jse(nitro:f("<figure><code>~n ~s~n~n</code></figure>",[forms:translate(FORM)]))),
      nitro:insert_bottom(stand, #p{body = [nitro:f("Size: ~p/BERT",[size(term_to_binary(FORM,[compressed]))]), "<br>",
                                            nitro:f("Type: ~p", [element(1,FORM)])]}), ok;

event(init) ->
    nitro:clear(stand),
    [ self() ! {client,{form,F}} || F <- lists:sort(mad_repl:wildcards(["src/forms/**/*.erl"])) ],
    ?LOG_INFO("HELO.~n",[]);

event({Event,Name}) ->
    nitro:wire(lists:concat(["console.log(\"",io_lib:format("~p",[{Event,Name}]),"\");"])),
    ?LOG_INFO("Event:~p.~n", [{Event,nitro:q(nitro:to_atom(Name))}]);

event(Event) ->
    ?LOG_INFO("Unknown:~p.~n", [Event]).
