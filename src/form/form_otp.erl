-module(form_otp).
-copyright('Maxim Sokhatsky').
-include("meta.hrl").
-include("doc.hrl").
-export(?EXP).
id() -> #phone{}.
doc() -> [].
new(Name,Phone, _) ->
  #document {
    name     = form:atom([otp,Name]),
    sections = [ #sec { name=[<<"Input the password you have received by SMS: "/utf8>> ] } ],
    buttons  = [ #but { id=decline,
                        name=decline,
                        title= <<"Cancel"/utf8>>,
                        class=cancel,
                        postback={'Close',[]} },
                 #but { id=proceed,
                        name=proceed,
                        title = <<"Proceed"/utf8>>,
                        class = [button,sgreen],
                        sources = [otp],
                        postback = {'Next',form:atom([otp,otp,Name])}}],
    fields = [ #field { id=otp,
                        name=otp,
                        type=otp,
                        title= <<"Password:"/utf8>>,
                        labelClass=label,
                        fieldClass=column3}]}.
