-module(forms).
-behaviour(application).
-autor('Maxim Sokhatsky').
-export([start/0, start/1, start/2, stop/1, new/2]).
-include_lib("n2o/include/wf.hrl").
-include("meta.hrl").

start() -> ok.
start(_) -> ok.
start(_,_) -> ok.
stop(_) -> ok.

new(Document,Object) ->
    [Section] = Document#document.sections,
    Fields    = Document#document.fields,
    Name      = Document#document.name,
    Buttons   = Document#document.buttons,
    #panel { id=wf:atom([form,Name]), class=form, body=[

        % caption

        #panel{class=caption,body=[#h3{body=Section#sec.name}]},

        % fields

        lists:foldr(    % empty

                    fun (#field{type=empty}=X1,Acc) ->
        [#panel{class=box,id=wf:atom([X1#field.name,Name])}|Acc];

                        % card row

                        (#field{type=card,name=[Name1,Name2,Name3]}=X2,Acc) ->
        [#panel { id=wf:atom([Name1,Name]), class=box, body=[
            #panel { class=label, body = X2#field.title},
            #panel { id=wf:atom([Name2,Name]), body=[#panel{class=field,
                     body = #select{id=wf:atom([Name3, Name]), disabled=true,
                            validation="validateCard(e)",
                            body= #option{selected=true, body= <<"Загрузка..."/utf8>>}}}]},
            #panel { class=tool, body= []}]}|Acc]; %#image{src="images/preloader.gif"}}]}|Acc];

                        % otp row

                        (#field{type=phone}=X3, Acc) ->
        [#panel { class=box, body=[
            #panel { class=label, body = X3#field.title},
            #panel { class=column3, body =
                #input{ class=phone,id=wf:atom([X3#field.name,Name]),
                        validation="validateNumbers(e, 4, 4, 'otp')",
                        onkeypress="return fieldsFilter(event, 4, 'otp');"} } ]} ];

                        % integer money combo sring

                        (#field{}=X,Acc) ->

                        Tooltips = [ #link { class=tooltips,
                                       tabindex="-1", onmouseover="setHeight(this);", body= [
                           #image { src= "images/question.png" },
                           #span  { body= Tx } ] } || Tx <- lists:reverse(X#field.tooltips) ],

                        Options = [ #label{body=
                           #radio{name=wf:atom([X#field.name,combo]),
                                  id=wf:atom([O#opt.name,Name]),
                                  body = O#opt.title,
                                  checked=O#opt.checked,postback={O#opt.name,Name}}}
                           || O <- X#field.options],

        [#panel { class=box, body=[
            #panel { class=label, body = X#field.title},
            #panel { class=field, body = case X#field.type of
                integer -> #b{body= wf:f(X#field.format,[
                                case X#field.postfun of
                                     [] -> element(X#field.pos,Object);
                                     PostFun -> PostFun(element(X#field.pos,Object)) end] )};
                money -> [ #input{ id=wf:atom([X#field.name,Name]),
                           validation=wf:f("validateSum(e, ~w, ~w, '~s')",[X#field.min,X#field.max, <<"Некорректная сумма!"/utf8>>]),
                           onkeypress=wf:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
                           value=wf:to_list(element(X#field.pos,Object)) },
                           #panel{ class=pt10,body= [ <<"Введите сумму не менее: 2 "/utf8>>, X#field.curr ] } ];
                combo -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                            lists:duplicate(length(Options),#br{}),Options)));
                string -> #input{ id=wf:atom([X#field.name,Name]),
                                  validation=wf:f("validateLength(e, ~w, ~w)",[X#field.min,X#field.max]),
                                  value=element(X#field.pos,Object)}
            end},
            #panel { class=tool, body= case Tooltips of
                                         [] -> [];
                                          _ -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                               lists:duplicate(length(Tooltips),#br{}),Tooltips)))
                                       end}
        ]}|Acc] end,[],Fields),

        % buttons

        #panel{id=forpreload,class=buttons,body= lists:foldr(fun(#but{}=But,Acc) ->
        [#link{class=But#but.class,postback=But#but.postback, body=But#but.title,
               source=[wf:atom([S,Name])||S<-But#but.sources]}|Acc] end,[],Buttons)}
    ]}.
