-module(forms_error).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").
-export([new/2]).

new({Operation,Code},Mess) ->
    ErrorMsg = forms:translate_error(Operation,Code),
    #panel{id=results, class=form, style="max-width:500px", body=[
        #panel{class=caption, body=[
            #h3{body= [<<"Error "/utf8>>, n2o:to_binary(Code), ": ", ErrorMsg]}]}
    ]}.
