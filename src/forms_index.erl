-module(forms_index).
-copyright("N2O Community").
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

main() -> [].
body() -> [].

event(init) ->
   [ begin

      Module = nitro:to_atom(filename:basename(F,".erl")),
      FORM   = Module:new([],Module:id()),
      NITRO  = forms:new(FORM, []),
      HTML5  = nitro:render(lists:flatten(nitro:render(NITRO))),
      Bin    = nitro:to_binary(HTML5),

      nitro:insert_bottom(stand, lists:flatten(nitro:to_list(nitro:render(#h3{body=Module})))),
      nitro:insert_bottom(stand, lists:flatten(nitro:to_list(Bin))),
      nitro:insert_bottom(stand, lists:flatten(nitro:jse(nitro:to_list("<figure><code>\n  "
                                                           ++ forms:translate(FORM) ++
                                                                 "\n\n</code></figure>"))))

    end || F <- lists:sort(mad_repl:wildcards(["src/forms/**/*.erl"])) ],
    io:format("HELO: OK~n");

event(Event) ->   n2o:info(?MODULE,"unknown: ~p",    [Event]).
