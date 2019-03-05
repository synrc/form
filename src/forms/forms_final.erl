-module(forms_final).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").
-include("meta.hrl").
-export(?EXP).
-record(otp, {error,code="OK"}).
id() -> #otp{}.
new(Name, O) ->
    #panel{id=results, class=form, body=[
        #panel{class=caption, body=[
            #h4{body= [<<"Success Operation: "/utf8>>, O#otp.code]}]},
        #panel{class=buttons, body=[
            #link{postback={confirmSuccess}, class=[button,sgreen],
                  body= "Confirm", source=[reason,type],
                       validate="preloadWithTimeout(this);"}
        ]}
    ]}.
