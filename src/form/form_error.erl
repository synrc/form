-module(form_error).
-copyright('Maxim Sokhatsky').
-include_lib("nitro/include/nitro.hrl").
-include("meta.hrl").
-export(?EXP).
-record(otp, {error="ERROR MSG",code="CODE"}).
id() -> #otp{}.
new(Name, #otp{error=ErrorMsg,code=Code}) ->
    #panel{id=form:atom([error,Name]), class=form, body=[
        #panel{class=caption, body=[
            #h4{body= [<<"Error "/utf8>>, n2o:to_binary(Code), ": ", ErrorMsg]}]}
    ]}.
