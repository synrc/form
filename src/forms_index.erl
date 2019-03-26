-module(forms_index).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

main() -> [].
body() -> [].

event({form,F}) ->
      nitro:actions([]),
      Module = nitro:to_atom(filename:basename(F,".erl")),
      FORM   = Module:new(Module,Module:id()),
      NITRO  = forms:new(FORM, []),
      HTML5  = nitro:render(lists:flatten(nitro:render(NITRO))),
      Bin    = nitro:to_binary(HTML5),
      nitro:insert_bottom(stand, lists:flatten(nitro:to_list(nitro:render(#h3{body=Module})))),
      nitro:insert_bottom(stand, #p{body = [nitro:f("Size: ~p/HTML ~p/BERT",
                                   [size(Bin),size(term_to_binary(FORM,[compressed]))]), "<br>",
                                    nitro:f("Type: ~p", [element(1,FORM)])]}),
      nitro:insert_bottom(stand, lists:flatten(nitro:to_list(Bin))),
      nitro:insert_bottom(stand, lists:flatten(nitro:jse(nitro:to_list("<figure><code>\n  "
                                                           ++ forms:translate(FORM) ++
                                                                 "\n\n</code></figure>"))));

event(init) ->
    nitro:clear(stand),
    [ self() ! {direct,{form,F}} || F <- lists:sort(mad_repl:wildcards(["src/forms/**/*.erl"])) ],
    n2o:info(?MODULE,"HELO: OK~n",[]);

event(Event) ->
    nitro:wire(lists:concat(["console.log(\"",io_lib:format("~p",[Event]),"\");"])),
    n2o:info(?MODULE,"Unknown: ~p~n", [{Event,nitro:q('otp_otp_forms_otp')}]).
