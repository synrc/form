-module(form_error).
-copyright('Maxim Sokhatsky').
-include_lib("nitro/include/nitro.hrl").
-include("meta.hrl").
-include("doc.hrl").
-export(?EXP).
id() -> #otp{}.
doc() -> [].
new(Name, O) -> new(Name, O, []).
new(Name, #otp{error=ErrorMsg,code=Code}, _) ->
    #panel{id=form:atom([error,Name]), class=form, body=[
        #panel{class=caption, body=[
            #h4{body= [<<"Error "/utf8>>, n2o:to_binary(Code), ": ", ErrorMsg]}]}
    ]}.
