-module(forms).
-behaviour(application).
-autor('Maxim Sokhatsky').
-export([start/0, start/1, start/2, stop/1, new/2]).
-export([form/2, steps/2, caption/2, fields/2, buttons/2, component/3]).
-include_lib("n2o/include/wf.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("step_wizard.hrl").
-include("meta.hrl").

start() -> ok.
start(_) -> ok.
start(_,_) -> {ok,self()}.
stop(_) -> ok.

new(Document,Object) ->
    Name   = Document#document.name,
    #panel{
        id=wf:atom([form,Name]), class=form,
        body= [BuildBlock(Document, Object) || BuildBlock <- [fun steps/2,fun caption/2,fun fields/2,fun buttons/2]]}.

form(Document,Object) ->
    Name       = Document#document.name,
    #panel { id=wf:atom([form,Name]), class=form}.

steps(Document, Object) ->
    % step wizard
    StepWizard = Document#document.steps,
    case StepWizard of
        #step_wizard{} ->
            #panel{class=steps, body=
            #b{body= [ wf:f(deposits:translate(step_wizard),
                [StepWizard#step_wizard.current_step,StepWizard#step_wizard.steps]) ]} };
        _ -> []
    end.

caption(Document, Object) ->
    % caption
    SectionList  = Document#document.sections,
    #panel{class=caption,body=
    [begin
         lists:flatten([#h3{ class=Section#sec.nameClass, body=Section#sec.name},
             #panel{ class=Section#sec.descClass, body = Section#sec.desc}]) end || Section <- SectionList]}.

fields(Document, Object) ->
    % fields
    Name       = Document#document.name,
    Fields     = Document#document.fields,
    lists:foldr(

        % empty
        fun (#field{type=empty}=X1,Acc) ->
            [#panel{class=box, style="display:none;",id=wf:atom([X1#field.name,Name])}|Acc];

            % card row
            (#field{type=card,name=[Name1,Name2,Name3]}=X2,Acc) ->
                [#panel { id=wf:atom([Name1,Name]), class=[box,pad0], body=[
                    #panel { class=label, body = X2#field.title},
                    #panel { class=field, style="width:66.63%", body=
                    #panel {id=wf:atom([Name2,Name]), body=[
                        #panel{class=field,style="width:90%;", body =
                        #select{id=wf:atom([Name3, Name]), disabled=true, validation="Validation.card(e)",
                            body= #option{selected=true, body= deposits:translate(loading)}}},
                        #panel { class=tool, body= [#image{src="/static/app/img/preloader.gif"}]} ]}} ]}|Acc];

            % comment for front manager
            (#field{type=comment}=X3,Acc) ->
                [#panel { id=commentBlock, class=box, body=[
                    case X3#field.tooltips of
                        false -> [];
                        true ->[#panel { class=label, body="&nbsp;" },
                            #panel { class=field, body="&nbsp;" },
                            #panel { class=tool, body=[
                                #link{id=wf:atom([commentlink,X3#field.name,Name]), class=tooltips, onclick=wf:f("showComment(this);"), body=[
                                    #image{src="/static/app/img/icon-comment-blue2.png"} ]} ]}] end,
                    #panel { class=comment, id=wf:atom([X3#field.name,Name]), body=[X3#field.desc],
                        style=case X3#field.tooltips of
                                  false -> "";
                                  true  -> "display:none;" end} ]}|Acc];

            % integer money combo sring
            (#field{}=X,Acc) ->
                Panel = case X#field.name of undefined -> #panel{};
                                             _         -> #panel{id=wf:atom([wrap,X#field.name,Name])} end,
                Tooltips = [
                    case Tx of
                        {N} -> #panel{id=wf:atom([tooltip,N,Name]), body=[]};
                        _ -> #link { class=tooltips, tabindex="-1", onmouseover="setHeight(this);", body=[
                                 #image { src= "/static/app/img/question.png" },
                                 #span  { body=Tx } ]} end || Tx <- lists:reverse(X#field.tooltips) ],

                Options = [ case {X#field.type, O#opt.noRadioButton} of
                                {_,true} -> #label{id=wf:atom([label,O#opt.name,Name]), body=[]};
                                {combo,_} -> #panel{style="height:30px;",body= #label{body=
                                                 #radio{name=wf:atom([X#field.name,combo]),
                                                        id=wf:atom([O#opt.name,Name]),
                                                        body = O#opt.title,
                                                        checked=O#opt.checked,
                                                        disabled=O#opt.disabled,
                                                        postback={O#opt.name,Name}}}};
                                {select,_} -> #option{value = O#opt.name, body = O#opt.title, selected = O#opt.checked};
                                {check,_}  -> #checkbox{id=wf:atom([O#opt.name,Name]),source=[wf:atom([O#opt.name,Name])],
                                                        body = O#opt.title, disabled=O#opt.disabled, checked=O#opt.checked, postback={O#opt.name,Name}} end || O <- X#field.options],

                [Panel#panel { class=X#field.boxClass, body=[
                    #panel { class=X#field.labelClass,  body = X#field.title},
                    #panel { class=X#field.fieldClass,
                        body = case X#field.type of
                                   text -> #panel{id=wf:atom([X#field.name,Name]), body=X#field.desc};
                                   integer -> #b{body= wf:f(X#field.format,
                                       [case X#field.postfun of
                                            [] -> element(X#field.pos,Object);
                                            PostFun -> PostFun(element(X#field.pos,Object)) end] )};
                                   money -> [ #input{ id=wf:atom([X#field.name,Name]), pattern="[0-9]*",
                                                      validation=wf:f("Validation.money(e, ~w, ~w, '~s')",[X#field.min,X#field.max, deposits:translate({?MODULE, error})]),
                                                      onkeypress=wf:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]), onkeyup="beautiful_numbers(event);",
                                                      value=wf:to_list(element(X#field.pos,Object)) },
                                              #panel{ class=pt10,body= [ deposits:translate({?MODULE, warning}), wf:to_binary(X#field.min), " ", X#field.curr ] } ];
                                   pay -> #panel{body=[#input{ id=wf:atom([X#field.name,Name]), pattern="[0-9]*",
                                                               validation=wf:f("Validation.pay(e, ~w, '~s')",[X#field.min,deposits:translate({?MODULE, error})]),
                                                               onkeypress=wf:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]), onkeyup="beautiful_numbers(event);",
                                                               value=wf:to_list(element(X#field.pos,Object)) }, <<" ">>, #span{body=X#field.curr}]};
                                   combo -> case length(Options) of
                                                1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                    lists:duplicate(length(Options),#br{}),Options)));
                                                _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                    lists:duplicate(length(Options),#panel{}),Options))) end;
                                   select -> #select{ id=wf:atom([X#field.name,Name]), postback=X#field.postback, onchange=X#field.onchange, body=Options};
                                   check  -> case length(Options) of
                                                 1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                     lists:duplicate(length(Options),#br{}),Options)));
                                                 _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                     lists:duplicate(length(Options),#panel{}),Options))) end;
                                   string -> #input{ class=dep_name,id=wf:atom([X#field.name,Name]),
                                                     validation=wf:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
                                                     onkeypress="return removeAllErrorsFromInput(this);",
                                                     value=element(X#field.pos,Object)};
                                   phone  -> [#span{ class=plus,body= <<"+">>},
                                       #input{ id=wf:atom([X#field.name,Name]), class=phone, pattern="[0-9]*",
                                               onkeypress=wf:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
                                               validation=wf:f("Validation.nums(e, ~w, ~w, 'phone')",[X#field.min,X#field.max]), value="380"} ];
                                   p24auth  -> [#span{class=plus,body= <<"&nbsp;&nbsp;">>},
                                       #input{ id=wf:atom([X#field.name,Name]), class=phone, type=password,
                                               onkeypress="return removeAllErrorsFromInput(this);",
                                               onkeyup="nextByEnter(event);",
                                               validation=wf:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
                                               placeholder=deposits:translate({p24auth, holder}) },
                                       #span{class=p24auth_link,body=
                                       [#link{href=deposits:translate({p24auth, lost_pass_link, deposits:getBank()}), target="_blank",
                                              postback=undefined, body= deposits:translate({p24auth, lost_pass}) }, #br{},
                                           #link{href=deposits:translate({p24auth, change_login_link, deposits:getBank()}), target="_blank",
                                               postback=undefined, body= deposits:translate({p24auth, change_login}) }]} ];
                                   otp    -> #input{ class=[phone,pass],id=wf:atom([X#field.name,Name]), placeholder="(XXXX)", pattern="[0-9]*",
                                                     validation="Validation.nums(e, 4, 4, 'otp')",
                                                     onkeypress="return fieldsFilter(event, 4, 'otp');"};
                                   datepay -> #panel{class=[field,sub,'input-date'], body=
                                   [#calendar{id=wf:atom([X#field.name,Name]), onkeypress="return fieldsFilter(event, 10, 'date');",
                                              validation="Validation.dateRegPay(e)", style="width:70%", disableDayFn="disableDays4Charge",
                                              class = ['input-date'], format="DD.MM.YYYY", minDate=X#field.min, lang=wf:lang(),value=element(X#field.pos,Object)},
                                       #span{class=['calendar-area','icon-calendar'],body=[]}]} end},

                    #panel { class=tool, body= case Tooltips of
                                                   [] -> [];
                                                   _ -> case length(Tooltips) of
                                                            1 -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                                                lists:duplicate(length(Tooltips),#br{}),Tooltips)));
                                                            _ -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                                                lists:duplicate(length(Tooltips),#panel{style="height:10px;"}),Tooltips))) end end} ]}|Acc] end,[],Fields).

buttons(Document, Object) ->
    % buttons
    Name       = Document#document.name,
    Buttons    = Document#document.buttons,
    #panel{id=forpreload,class=buttons,body= lists:foldr(fun(#but{}=But,Acc) ->
        [#link{id=But#but.id, class=But#but.class, validate=But#but.validation, postback=But#but.postback, body=But#but.title, onclick=But#but.onclick,
               href=But#but.href, target=But#but.target, source=[wf:atom([S,Name])||S<-But#but.sources]}|Acc] end,[],Buttons)}.

component(FormId, Document, Object) ->
    Name   = Document#document.name,
    #panel{id=wf:atom([FormId,Name]), body= [BuildBlock(Document, Object) || BuildBlock <- [fun fields/2]]}.
