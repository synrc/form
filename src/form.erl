-module(form).
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/0, start/1, start/2, stop/1, new/2, init/1, id/0]).
-export([form/2, steps/2, caption/2, fields/2, buttons/2, component/3]).
-include_lib("nitro/include/nitro.hrl").
-include_lib("form/include/step_wizard.hrl").
-include_lib("form/include/meta.hrl").

id() -> fun (X) -> X end.

translate_error(A,B) -> io_lib:format("~p",[{A,B}]).
translate(A,B)       -> io_lib:format("~p",[{A,B}]).
translate(A)         -> io_lib:format("~p",[A]).

stop(_)    -> ok.
main(A)    -> mad:main(A).
start()    -> start(normal,[]).
start(_)   -> start().
init([])   -> {ok, {{one_for_one, 5, 10}, [] }}.
start(_,_) -> supervisor:start_link({local,form},form,[]).

atom(List) when is_list(List) -> string:join([ nitro:to_list(L) || L <- List],"_");
atom(Scalar) -> nitro:to_list(Scalar).

new(Document = #document{},Object) ->
    Name = Document#document.name,
    #panel{
        id=form:atom([form,Name]), class=form,
        body= [ BuildBlock(Document, Object)
             || BuildBlock <- [fun steps/2,fun caption/2,fun fields/2,fun buttons/2]]};

new(Document,Object) -> Document. % pass HTML5 as NITRO records

form(Document,_Object) ->
    Name = Document#document.name,
    #panel { id=form:atom([form,Name]), class=form}.

steps(Document, _Object) ->
    StepWizard = Document#document.steps,
    case StepWizard of
        #step_wizard{} ->
            #panel{class=steps, body=
            #b{body= [ nitro:f(translate(step_wizard),
                [StepWizard#step_wizard.current_step,StepWizard#step_wizard.steps]) ]} };
        _ -> []
    end.

caption(Document, _Object) ->
    SectionList  = Document#document.sections,
    #panel{class=caption,body=
         [begin
            [#h4{ class=Section#sec.nameClass, body=Section#sec.name},
             #panel{ class=Section#sec.descClass, body = Section#sec.desc}]
          end || Section <- SectionList]}.

fields(Document, Object) ->
    Name       = Document#document.name,
    Fields     = Document#document.fields,
    lists:foldr(fun (X,Acc) -> fieldType(X,Acc,Object) end,[],Fields).

buttons(Document, _Object) ->
    Name    = Document#document.name,
    Buttons  = Document#document.buttons,
    #panel{ id = forpreload, class = buttons,
            body= lists:foldr(fun(#but{}=But,Acc) ->
                [ #link { id=But#but.id, class=But#but.class,
                          validate=But#but.validation, postback=But#but.postback,
                          body=But#but.title, onclick=But#but.onclick,
                          href=But#but.href, target=But#but.target,
                          source=But#but.sources}|Acc] end, [], Buttons)}.

component(FormId, Document, Object) ->
    Name = Document#document.name,
    #panel{ id=form:atom([FormId,Name]),
            body= [BuildBlock(Document, Object) || BuildBlock <- [fun fields/2]]}.

% GENERIC MATCH

fieldType(#field{type=empty}=X,Acc,Object) ->
    [#panel{class=box, style="display:none;",id=form:atom([X#field.name,empty])}|Acc];

fieldType(#field{type=dynamic}=X,Acc,Object) ->
    [X#field.raw|Acc];

fieldType(#field{type=comment}=X,Acc,Object) ->
    [#panel { id=commentBlock, class=box, body=[
         case X#field.tooltips of
              false -> [];
              true -> [ #panel { class=label, body="&nbsp;" },
                        #panel { class=field, body="&nbsp;" },
                        #panel { class=tool, body=[
                         #link { id=form:atom([commentlink,X#field.name]),
                                 class=tooltips, onclick=nitro:f("showComment(this);"),
                                 body=[#image{src="/app/img/icon-comment-blue2.png"} ]} ]}] end,
                        #panel { class=comment, id=form:atom([X#field.name]),
                                 body=[X#field.desc],
                                 style= case X#field.tooltips of
                                             false -> "";
                                             true  -> "display:none;" end} ]}|Acc];

fieldType(#field{type=card}=X,Acc,Object) ->
   [#panel { id=form:atom([card,X#field.name]), class=[box,pad0], body=[
             #panel { class=label, body = X#field.title},
             #panel { class=field, style="width:66.63%", body=
                       #panel { id=form:atom([X#field.name,1]), body=[
                       #panel { class=field,style="width:90%;", body =
                      #select { id=form:atom([X#field.name,2]), disabled=true,
                                validation="Validation.card(e)",
                                body= #option{selected=true, body= form:translate(loading)}}},
                       #panel { class=tool, body= [#image{src="/app/img/preloader.gif"}]} ]}} ]}|Acc];

fieldType(#field{}=X,Acc,Object) ->
   Panel = case X#field.name of [] -> #panel{};
                                 _ -> #panel{id=form:atom([wrap,X#field.name])} end,
   Tooltips =
   [ case Tx of
          {N} -> #panel{ id=form:atom([tooltip,N]), body=[]};
          _ -> #link { class=tooltips, tabindex="-1", onmouseover="setHeight(this);",
                       body=[ #image { src= "app/img/question.png" },
                              #span  { body=Tx } ]} end || Tx <- lists:reverse(X#field.tooltips) ],

   Options =
   [ case {X#field.type, O#opt.noRadioButton} of
          {_,true} -> #label{id=form:atom([label,O#opt.name]), body=[]};
          {select,_} -> #option{value = O#opt.name, body = O#opt.title, selected = O#opt.checked};
          {combo,_} -> #panel{body= #label{body= #radio{name=form:atom([X#field.name,combo]),
                          id=form:atom([O#opt.name]), body = O#opt.title,
                          checked=O#opt.checked, disabled=O#opt.disabled, postback={O#opt.name}}}};
         {check,_}  -> #checkbox{id=form:atom([O#opt.name]),source=[form:atom([O#opt.name])],
                          body = O#opt.title, disabled=O#opt.disabled, checked=O#opt.checked,
                          postback={O#opt.name}} end || O <- X#field.options ],

   [ Panel#panel { class=X#field.boxClass, body=[
          #panel { class=X#field.labelClass, body = X#field.title},
          #panel { class=X#field.fieldClass, body = fieldType(X#field.type,X,Options,Object)},
          #panel { class=tool, body= case Tooltips of
                   [] -> [];
                    _ -> case length(Tooltips) of
                              1 -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                      lists:duplicate(length(Tooltips),#br{}),Tooltips)));
                              _ -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                      lists:duplicate(length(Tooltips),#panel{style="height:10px;"}),
                                      Tooltips))) end end} ]}|Acc].

% SECOND LEVEL MATCH

fieldType(text,X,Options,Object) ->
    #panel{id=X#field.name, body=X#field.desc};

fieldType(integer,X,Options,Object) ->
   #b{body= nitro:f(X#field.format,
           [ case X#field.postfun of
                  [] -> element(X#field.pos,Object);
                  PostFun -> PostFun(element(X#field.pos,Object)) end] )};

fieldType(money,X,Options,Object) ->
 [ #input{ id=X#field.name, pattern="[0-9]*",
           validation=nitro:f("Validation.money(e, ~w, ~w, '~s')",
             [X#field.min,X#field.max, form:translate({?MODULE, error})]),
           onkeyup="beautiful_numbers(event);",
           value=nitro:to_list(element(X#field.pos,Object)) },
           #panel{ class=pt10,body= [ form:translate({?MODULE, warning}),
                                      nitro:to_binary(X#field.min), " ", X#field.curr ] } ];

fieldType(pay,X,Options,Object) ->
   #panel{body=[#input{ id=X#field.name, pattern="[0-9]*",
                        validation=X#field.validation,
                        onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",
                           [X#field.length,X#field.type]),
                        onkeyup="beautiful_numbers(event);",
                        value=nitro:to_list(element(X#field.pos,Object)) }, <<" ">>,
                 #span{ body=X#field.curr}]};

fieldType(combo,X,Options,Object) ->
   case length(Options) of
        1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#br{}),Options)));
        _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#panel{}),Options))) end;

fieldType(select,X,Options,Object) ->
   #select{ id=X#field.name, postback=X#field.postback, body=Options};

fieldType(check,X,Options,Object) ->
   case length(Options) of
        1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#br{}),Options)));
        _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#panel{}),Options))) end;

fieldType(string,X,Options,Object) ->
   #input{ class=dep_name,id=X#field.name,
           validation=nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
%          onkeypress="return removeAllErrorsFromInput(this);",
           value=element(X#field.pos,Object)};

fieldType(phone,X,Options,Object) ->
   #input{ id=X#field.name, class=phone, pattern="[0-9]*",
           onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
           validation=nitro:f("Validation.phone(e, ~w, ~w)",[X#field.min,X#field.max]), value="+380"};

fieldType(auth,X,Options,Object) ->
 [ #input{ id=X#field.name, class=phone, type=password,
           onkeypress="return removeAllErrorsFromInput(this);",
           onkeyup="nextByEnter(event);",
           validation=nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max]),
           placeholder= form:translate({auth, holder}) },
    #span{ class=auth_link,
           body = [ #link{ href= form:translate({auth, lost_pass_link}),
                           target="_blank", postback=undefined,
                           body= form:translate({auth, lost_pass}) }, #br{},
                    #link{ href= form:translate({auth, change_login_link}),
                           target="_blank", postback=undefined,
                           body= form:translate({auth, change_login}) }]} ];

fieldType(otp,X,Options,Object) ->
   #input{ class=[phone,pass],id=X#field.name, placeholder="(XXXX)", pattern="[0-9]*",
           validation="Validation.nums(e, 4, 4, \"otp\")"
         };

fieldType(calendar,X,Options,Object) ->
   #panel{class=[field],
          body=[#calendar{id=X#field.name,
          validation="Validation.dateRegPay(e)", disableDayFn="disableDays4Charge",
          class = ['input-date'], minDate=X#field.min, maxDate = X#field.max, lang=ua}]}.
