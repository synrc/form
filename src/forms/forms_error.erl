-module(forms_error).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").
-include("meta.hrl").
-export(?EXP).
-record(otp, {error="ERROR MSG",code="CODE"}).
id() -> #otp{}.
new(Name, #otp{error=ErrorMsg,code=Code}) ->
    #panel{id=results, class=form, body=[
        #panel{class=caption, body=[
            #h4{body= [<<"Error "/utf8>>, n2o:to_binary(Code), ": ", ErrorMsg]}]}
    ]}.
