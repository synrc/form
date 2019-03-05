-module(forms_final).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").
-export([new/0]).

new() ->
    #panel{id=results, class=form, style="max-width:500px", body=[
        #panel{class=caption, body=[
            #h3{body= <<"Success Operation"/utf8>>}]},

        #panel{class=buttons, body=[
            #link{postback={admin_bo_rastr,backToListAfterFinish}, class=[button,sgreen],
                  body=forms:translate(proceed,ru), source=[reason,type],
                       validate="preloadWithTimeout(this);"}
        ]}
    ]}.
