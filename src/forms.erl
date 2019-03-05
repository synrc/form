-module(forms).
-behaviour(supervisor).
-behaviour(application).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-export([start/0, start/1, start/2, stop/1, new/2, init/1]).
-export([form/2, steps/2, caption/2, fields/2, buttons/2, component/3]).
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/n2o.hrl").
-include("step_wizard.hrl").
-include("meta.hrl").

translate_error(A,B) -> io_lib:format("~p",[{A,B}]).
translate(A,B)       -> io_lib:format("~p",[{A,B}]).
translate(A)         -> io_lib:format("~p",[A]).

stop(_)    -> ok.
main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_)   -> start().
init([])   -> {ok, {{one_for_one, 5, 10}, [] }}.
start(_,_) -> cowboy:start_clear(http, [{port, 8002}],
              #{ env => #{dispatch => n2o_cowboy2:points()} }),
              supervisor:start_link({local,forms},forms,[]).

atom(List) when is_list(List) -> nitro:to_atom(string:join([ nitro:to_list(L) || L <- List],"_"));
atom(Scalar) -> nitro:to_atom(Scalar).

new(Document = #document{},Object) ->
    Name   = Document#document.name,
    #panel{
        id=forms:atom([form,Name]), class=form,
        body= [BuildBlock(Document, Object)
        || BuildBlock <- [fun steps/2,fun caption/2,fun fields/2,fun buttons/2]]};
new(Document,Object) -> Document.

form(Document,_Object) ->
    Name       = Document#document.name,
    #panel { id=forms:atom([form,Name]), class=form}.

steps(Document, _Object) ->
    % step wizard
    StepWizard = Document#document.steps,
    case StepWizard of
        #step_wizard{} ->
            #panel{class=steps, body=
            #b{body= [ nitro:f(translate(step_wizard),
                [StepWizard#step_wizard.current_step,StepWizard#step_wizard.steps]) ]} };
        _ -> []
    end.

caption(Document, _Object) ->
    % caption
    SectionList  = Document#document.sections,
    #panel{class=caption,body=
    [begin
         lists:flatten([#h4{ class=Section#sec.nameClass, body=Section#sec.name},
             #panel{ class=Section#sec.descClass, body = Section#sec.desc}]) end
             || Section <- SectionList]}.

fields(Document, Object) ->
    % fields
    Name       = Document#document.name,
    Fields     = Document#document.fields,
    lists:foldr(

        % empty
        fun (#field{type=empty}=X1,Acc) ->
            [#panel{class=box, style="display:none;",id=forms:atom([X1#field.name,Name])}|Acc];

            %% dynamic component (that is build with fun component/3 and inserted)
            (#field{type=dynamic}=X4,Acc) -> [X4#field.raw|Acc];

            % card row
            (#field{type=card,name=[Name1,Name2,Name3]}=X2,Acc) ->
                [#panel { id=forms:atom([Name1,Name]), class=[box,pad0], body=[
                    #panel { class=label, body = X2#field.title},
                    #panel { class=field, style="width:66.63%", body=
                    #panel {id=forms:atom([Name2,Name]), body=[
                        #panel{class=field,style="width:90%;", body =
                        #select{id=forms:atom([Name3, Name]), disabled=true, validation="Validation.card(e)",
                            body= #option{selected=true, body= forms:translate(loading)}}},
                        #panel { class=tool, body= [#image{src="/static/app/img/preloader.gif"}]} ]}} ]}|Acc];

            % comment for front manager
            (#field{type=comment}=X3,Acc) ->
                [#panel { id=commentBlock, class=box, body=[
                    case X3#field.tooltips of
                        false -> [];
                        true ->[#panel { class=label, body="&nbsp;" },
                            #panel { class=field, body="&nbsp;" },
                            #panel { class=tool, body=[
                                #link{id=forms:atom([commentlink,X3#field.name,Name]), class=tooltips, onclick=nitro:f("showComment(this);"), body=[
                                    #image{src="/static/app/img/icon-comment-blue2.png"} ]} ]}] end,
                    #panel { class=comment, id=forms:atom([X3#field.name,Name]), body=[X3#field.desc],
                        style=case X3#field.tooltips of
                                  false -> "";
                                  true  -> "display:none;" end} ]}|Acc];

            % integer money combo sring
            (#field{}=X,Acc) ->
                Panel = case X#field.name of undefined -> #panel{};
                                             _         -> #panel{id=forms:atom([wrap,X#field.name,Name])} end,
                Tooltips = [
                    case Tx of
                        {N} -> #panel{id=forms:atom([tooltip,N,Name]), body=[]};
                        _ -> #link { class=tooltips, tabindex="-1", onmouseover="setHeight(this);", body=[
                                 #image { src= "/static/app/img/question.png" },
                                 #span  { body=Tx } ]} end || Tx <- lists:reverse(X#field.tooltips) ],

                Options = [ case {X#field.type, O#opt.noRadioButton} of
                                {_,true} -> #label{id=forms:atom([label,O#opt.name,Name]), body=[]};
                                {combo,_} -> #panel{style="height:30px;",body= #label{body=
                                                 #radio{name=forms:atom([X#field.name,combo]),
                                                        id=forms:atom([O#opt.name,Name]),
                                                        body = O#opt.title,
                                                        checked=O#opt.checked,
                                                        disabled=O#opt.disabled,
                                                        postback={O#opt.name,Name}}}};
                                {select,_} -> #option{value = O#opt.name, body = O#opt.title, selected = O#opt.checked};
                                {check,_}  -> #checkbox{id=forms:atom([O#opt.name,Name]),source=[forms:atom([O#opt.name,Name])],
                                                        body = O#opt.title, disabled=O#opt.disabled, checked=O#opt.checked, postback={O#opt.name,Name}} end || O <- X#field.options],

                [Panel#panel { class=X#field.boxClass, body=[
                    #panel { class=X#field.labelClass,  body = X#field.title},
                    #panel { class=X#field.fieldClass,
                        body = case X#field.type of
                                   text -> #panel{id=forms:atom([X#field.name,Name]), body=X#field.desc};
                                   integer -> #b{body= nitro:f(X#field.format,
                                       [case X#field.postfun of
                                            [] -> element(X#field.pos,Object);
                                            PostFun -> PostFun(element(X#field.pos,Object)) end] )};
                                   money -> [ #input{ id=forms:atom([X#field.name,Name]), pattern="[0-9]*",
                                                      validation=nitro:f("Validation.money(e, ~w, ~w, '~s')",[X#field.min,X#field.max, forms:translate({?MODULE, error})]),
                                                      onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]), onkeyup="beautiful_numbers(event);",
                                                      value=nitro:to_list(element(X#field.pos,Object)) },
                                              #panel{ class=pt10,body= [ forms:translate({?MODULE, warning}), nitro:to_binary(X#field.min), " ", X#field.curr ] } ];
                                   pay -> #panel{body=[#input{ id=forms:atom([X#field.name,Name]), pattern="[0-9]*",
                                                               validation=X#field.validation,
                                                               onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
                                                               onkeyup="beautiful_numbers(event);",
                                                               value=nitro:to_list(element(X#field.pos,Object)) }, <<" ">>, #span{body=X#field.curr}]};
                                   combo -> case length(Options) of
                                                1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                    lists:duplicate(length(Options),#br{}),Options)));
                                                _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                    lists:duplicate(length(Options),#panel{}),Options))) end;
                                   select -> #select{ id=forms:atom([X#field.name,Name]), postback=X#field.postback, onchange=X#field.onchange, body=Options};
                                   check  -> case length(Options) of
                                                 1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                     lists:duplicate(length(Options),#br{}),Options)));
                                                 _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                                                     lists:duplicate(length(Options),#panel{}),Options))) end;
                                   string -> #input{ class=dep_name,id=forms:atom([X#field.name,Name]),
                                                     validation=nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
                                                     onkeypress="return removeAllErrorsFromInput(this);",
                                                     value=element(X#field.pos,Object)};
                                   phone  -> #input{ id=forms:atom([X#field.name,Name]), class=phone, pattern="[0-9]*",
                                                     onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
                                                     validation=nitro:f("Validation.phone(e, ~w, ~w)",[X#field.min,X#field.max]), value="+380"};
                                   auth -> [
                                       #input{ id=forms:atom([X#field.name,Name]), class=phone, type=password,
                                               onkeypress="return removeAllErrorsFromInput(this);",
                                               onkeyup="nextByEnter(event);",
                                               validation=nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
                                               placeholder= forms:translate({auth, holder}) },
                                       #span{class=auth_link,body=
                                       [#link{href= forms:translate({auth, lost_pass_link}), target="_blank",
                                              postback=undefined, body= forms:translate({auth, lost_pass}) }, #br{},
                                           #link{href= forms:translate({auth, change_login_link}), target="_blank",
                                               postback=undefined, body= forms:translate({auth, change_login}) }]} ];
                                   otp    -> #input{ class=[phone,pass],id=forms:atom([X#field.name,Name]), placeholder="(XXXX)", pattern="[0-9]*"
%                                                     validation="Validation.nums(e, 4, 4, 'otp')",
%                                                     onkeypress="return fieldsFilter(event, 4, 'otp');"
                                                     };
                                   datepay -> #panel{class=[field,sub,'input-date'], body=
                                   [#calendar{id=forms:atom([X#field.name,Name]), onkeypress="return fieldsFilter(event, 10, 'date');",
                                              validation="Validation.dateRegPay(e)", style="width:70%", disableDayFn="disableDays4Charge",
                                              class = ['input-date'], format="DD.MM.YYYY", minDate=X#field.min, maxDate = X#field.max, lang=en,value=element(X#field.pos,Object)},
                                       #span{class=['calendar-area','icon-calendar'],body=[]}]} end},

                    #panel { class=tool, body= case Tooltips of
                                                   [] -> [];
                                                   _ -> case length(Tooltips) of
                                                            1 -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                                                lists:duplicate(length(Tooltips),#br{}),Tooltips)));
                                                            _ -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                                                lists:duplicate(length(Tooltips),#panel{style="height:10px;"}),Tooltips))) end end} ]}|Acc] end,[],Fields).

buttons(Document, _Object) ->
    % buttons
    Name       = Document#document.name,
    Buttons    = Document#document.buttons,
    #panel{id=forpreload,class=buttons,body= lists:foldr(fun(#but{}=But,Acc) ->
        [#link{id=But#but.id, class=But#but.class, validate=But#but.validation, postback=But#but.postback, body=But#but.title, onclick=But#but.onclick,
               href=But#but.href, target=But#but.target, source=[forms:atom([S,Name])||S<-But#but.sources]}|Acc] end,[],Buttons)}.

component(FormId, Document, Object) ->
    Name   = Document#document.name,
    #panel{id=forms:atom([FormId,Name]), body= [BuildBlock(Document, Object) || BuildBlock <- [fun fields/2]]}.
