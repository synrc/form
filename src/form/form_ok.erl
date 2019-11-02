-module(form_ok).
-copyright('Maxim Sokhatsky').
-include_lib("nitro/include/nitro.hrl").
-include("meta.hrl").
-include("doc.hrl").
-export(?EXP).
id() -> #otp{}.
doc() -> [].
new(Name, O, _) ->
    #panel{id=form:atom([ok,Name]), class=form, body=[
        #panel{class=caption, body=[
            #h4{body= [<<"Success Operation: "/utf8>>, O#otp.code]}]},
        #panel{class=buttons, body=[
            #link{postback={confirmSuccess}, class=[button,sgreen],
                  body= "Confirm", source=[],
                       validate=[]}
        ]}
    ]}.
