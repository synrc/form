-module(forms_otp).
-copyright('Maxim Sokhatsky').
-include("meta.hrl").
-export(?EXP).
-record(phone, {code="+380",number="490000000"}).
id() -> #phone{}.
new(Name,Phone) ->

  #document { name = Name, sections = [
      #sec { name=[<<"Input the password "
                     "you have received by SMS: "/utf8>>,
             nitro:to_list(Phone#phone.code),
             nitro:to_list(Phone#phone.number)
             ] } ],

    buttons  = [ #but { name='decline',
                        title= <<"Cancel"/utf8>>,
                        class=cancel,
                        postback={'CloseOpenedForm',Name} },

                 #but { name='proceed',
                        title = <<"Proceed"/utf8>>,
                        class = [button,sgreen],
                        sources = [otp],
                        postback = {'Spinner',{'OpenForm',Name}}}],

    fields = [ #field { name='otp',
                        type=otp,
                        title= <<"Password:"/utf8>>,
                        labelClass=label,
                        fieldClass=column3}]}.
