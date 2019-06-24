-module(form_ok).
-copyright('Maxim Sokhatsky').
-include_lib("nitro/include/nitro.hrl").
-include("meta.hrl").
-export(?EXP).
-record(otp, {error,code="OK"}).
id() -> #otp{}.
new(Name, O) ->
    #panel{id=form:atom([ok,Name]), class=form, body=[
        #panel{class=caption, body=[
            #h4{body= [<<"Success Operation: "/utf8>>, O#otp.code]}]},
        #panel{class=buttons, body=[
            #link{postback={confirmSuccess}, class=[button,sgreen],
                  body= "Confirm", source=[],
                       validate=[]}
        ]}
    ]}.
