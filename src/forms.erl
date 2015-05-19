-module(forms).
-behaviour(application).
-autor('Maxim Sokhatsky').
-export([start/0, start/1, start/2, stop/1, new/2]).
-include_lib("n2o/include/wf.hrl").
-include_lib("step_wizard.hrl").
-include("meta.hrl").

start() -> ok.
start(_) -> ok.
start(_,_) -> {ok,self()}.
stop(_) -> ok.

new(Document,Object) ->
    StepWizard = Document#document.steps,
    [Section]  = Document#document.sections,
    Fields     = Document#document.fields,
    Name       = Document#document.name,
    Buttons    = Document#document.buttons,
    #panel { id=wf:atom([form,Name]), class=form, body=[

        % step wizard

        case StepWizard of
            #step_wizard{} ->
                #panel{class=steps, body=
                #b{body= [ wf:f(deposits:translate(step_wizard),
                    [StepWizard#step_wizard.current_step,StepWizard#step_wizard.steps]) ]} };
            _ -> []
        end,

        % caption

        #panel{class=caption,body=[
              #h3{ class=Section#sec.nameClass, body=Section#sec.name},
              #panel{ class=Section#sec.descClass, body = Section#sec.desc}
        ]},


        % fields

        lists:foldr(    % empty

                    fun (#field{type=empty}=X1,Acc) ->
        [#panel{class=box, style="display:none;",id=wf:atom([X1#field.name,Name])}|Acc];

                        % card row

                        (#field{type=card,name=[Name1,Name2,Name3]}=X2,Acc) ->
        [#panel { id=wf:atom([Name1,Name]), style="padding-top:0;", class=box, body=[
            #panel { class=label, body = X2#field.title},
            #panel { class=field, style="width:66.63%", body=
                #panel {class=box, id=wf:atom([Name2,Name]), body=[
                    #panel{class=field,style="width:90%;", body =
                        #select{id=wf:atom([Name3, Name]), disabled=true, validation="Validation.card(e)",
                                body= #option{selected=true, body= deposits:translate(loading)}}},
                    #panel { class=tool, body= [#image{src="/static/app/img/preloader.gif"}]}
                ]}}
         ]}|Acc];

                        % integer money combo sring

                        (#field{}=X,Acc) ->

                        Tooltips = [
                            case Tx of
                                {comment} -> #link{id=wf:atom([commlink,X#field.name,Name]), class=tooltips, onclick=wf:f("showComment();"), body=[
                                                    #image{id=wf:atom([commlink1,X#field.name,Name]), src="/static/app/img/icon-comment-blue2.png"}]};
                                {N} -> #panel{id=wf:atom([tooltip,N,Name]), body=[]};
                                _ -> #link{class=tooltips, tabindex="-1", onmouseover="setHeight(this);", body=[
                                        #image{src="/static/app/img/question.png"},
                                        #span{body=Tx} ]}
                            end
                            || Tx <- lists:reverse(X#field.tooltips) ],

                        Options = [
                            case O#opt.noRadioButton of
                                true -> #label{id=wf:atom([label,O#opt.name,Name]), body=[]};
                                false -> #panel{style="height:30px;",body= #label{body=
                                            #radio{name=wf:atom([X#field.name,combo]),
                                                id=wf:atom([O#opt.name,Name]),
                                                body = O#opt.title,
                                                checked=O#opt.checked,
                                                disabled=O#opt.disabled,
                                                postback={O#opt.name,Name}}}}
                            end
                            || O <- X#field.options],

        [#panel { class=box, body=[
            #panel { class=X#field.labelClass,  body = X#field.title},
            #panel { class=X#field.fieldClass, body = case X#field.type of
                text -> #panel{body=X#field.desc};
                comment -> #panel{id=wf:atom([X#field.name,Name]), body=X#field.desc};
                integer -> #b{body= wf:f(X#field.format,[
                                case X#field.postfun of
                                     [] -> element(X#field.pos,Object);
                                     PostFun -> PostFun(element(X#field.pos,Object)) end] )};
                money -> [ #input{ id=wf:atom([X#field.name,Name]),
                           validation=wf:f("Validation.money(e, ~w, ~w, '~s')",[X#field.min,X#field.max, deposits:translate({?MODULE, error})]),
                           onkeypress=wf:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]), onkeyup="beautiful_numbers(event);",
                           value=wf:to_list(element(X#field.pos,Object)) },
                           #panel{ class=pt10,body= [ deposits:translate({?MODULE, warning}), X#field.curr ] } ];
                combo -> case length(Options) of
                             1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                     lists:duplicate(length(Options),#br{}),Options)));
                             _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                     lists:duplicate(length(Options),#panel{}),Options)))
                         end;
                string -> #input{ class=dep_name,id=wf:atom([X#field.name,Name]),
                                  validation=wf:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
                                  onkeypress="return removeAllErrorsFromInput(this);",
                                  value=element(X#field.pos,Object)};
                phone  -> #input{ id=wf:atom([X#field.name,Name]), class=phone,
                                  onkeypress=wf:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
                                  validation=wf:f("Validation.nums(e, ~w, ~w, 'phone')",[X#field.min,X#field.max]),
                                  value="380"};
                otp    -> #input{ class=[phone,pass],id=wf:atom([X#field.name,Name]), placeholder="(XXXX)",
                                  validation="Validation.nums(e, 4, 4, 'otp')",
                                  onkeypress="return fieldsFilter(event, 4, 'otp');"}
            end},
            #panel { class=tool, body= case Tooltips of
                                         [] -> [];
                                          _ -> case length(Tooltips) of
                                                   1 -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                                        lists:duplicate(length(Tooltips),#br{}),Tooltips)));
                                                   _ -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                                       lists:duplicate(length(Tooltips),#panel{style="height:10px;"}),Tooltips)))
                                               end
                                       end}
        ]}|Acc] end,[],Fields),

        % buttons

        #panel{id=forpreload,class=buttons,body= lists:foldr(fun(#but{}=But,Acc) ->
        [#link{class=But#but.class,postback=But#but.postback, body=But#but.title,
               source=[wf:atom([S,Name])||S<-But#but.sources]}|Acc] end,[],Buttons)}
    ]}.
