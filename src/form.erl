-module(form).
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/0, start/1, start/2, stop/1, new/2, new/3, init/1, id/0, dispatch/2]).
-export([steps/3, caption/3, fields/3, buttons/3]).
-include_lib("nitro/include/calendar.hrl").
-include_lib("nitro/include/comboLookup.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("form/include/formReg.hrl").
-include_lib("form/include/step_wizard.hrl").
-include_lib("form/include/meta.hrl").

sources(Object) ->
   M = lists:map(fun(X) -> list_to_atom(atom([X,type(Object)])) end, element(5,kvs:table(type(Object)))),
%   io:format("sources: ~p~n",[M]),
   M.

type(O) -> element(1,O).

pos(Object,X) ->
   Tab = element(1,Object),
   Fields = element(5,kvs:table(Tab)),
   P = string:rstr([Tab|Fields],[X#field.id]),
%   io:format("pos: ~p~n",[{Object,X}]),
   P.

extract(Object,X) ->
%   io:format("extract: ~p~n",[{Object,X}]),
   element(pos(Object,X),Object).

evoke(Object,X,Value) ->
   setelement(pos(Object,X),Object,Value).

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

dispatch(Object, Options) ->
   Name = proplists:get_value(name,Options,false),
   View = proplists:get_value(view,Options,false),
   Edit = proplists:get_value(edit,Options,false),
   New  = proplists:get_value(new,Options,false),
   Search = proplists:get_value(search,Options,false),
   Row = proplists:get_value(row,Options,false),
   Registry = application:get_env(form,registry,[]),
   #formReg{vertical = V, horizontal = H} = lists:keyfind(element(1,Object),2,Registry:registry()),
   case {New,Search,Edit,View,Row} of
      {true,_,_,_,_} ->  new(V:new(nitro:compact(Name),Object,Options),Object,Options);
      {_,true,_,_,_} ->  new(V:new(nitro:compact(Name),Object,Options),Object,Options);
      {_,_,true,_,_} ->  new(V:new(nitro:compact(Name),Object,Options),Object,Options);
      {_,_,_,true,_} ->  new(V:new(nitro:compact(Name),Object,Options),Object,Options);
      {_,_,_,_,true} ->  new(H:new(nitro:compact(Name),Object,Options),Object,Options)
   end.

new(Document = #document{},Object,Opt) ->
    Name = Document#document.name,
    #panel{
        id=atom([form,Name]), class=form,
        body= [ BuildBlock(Document, Object, Opt)
             || BuildBlock <- [fun steps/3,fun caption/3,fun fields/3,fun buttons/3]]};

new(Document,_Object,_Opt) -> Document. % pass HTML5 as NITRO records

new(A,B) -> new(A,B,[]).

steps(Document, _Object, _Opt) ->
    StepWizard = Document#document.steps,
    case StepWizard of
        #step_wizard{} ->
            #panel{class=steps, body= ["<b>", nitro:f(translate(step_wizard),
                [StepWizard#step_wizard.current_step,StepWizard#step_wizard.steps]), "</b>" ] };
        _ -> []
    end.

caption(Document, _Object, _Opt) ->
    SectionList  = Document#document.sections,
    #panel{class=caption,body=
         [begin
            [#h4{ class=Section#sec.nameClass, body=Section#sec.name},
             #panel{ class=Section#sec.descClass, body = Section#sec.desc}]
          end || Section <- SectionList]}.

fields(Document, Object, Opt) ->
    Name       = Document#document.name,
    Fields     = Document#document.fields,
    lists:foldr(fun (X,Acc) -> fieldType(X,Acc,Object,Opt) end,[],Fields).

buttons(Document, Object, _Opt) ->
    Name    = Document#document.name,
    Buttons  = Document#document.buttons,
    #panel{ id = forpreload, class = buttons,
            body= lists:foldr(fun(#but{}=But,Acc) ->
                [ #link { id= atom([But#but.id,type(Object)]), class=But#but.class,
                          validate=But#but.validation,
                          postback=But#but.postback,
                          body=But#but.title, onclick=But#but.onclick,
                          href=But#but.href, target=But#but.target,
                          source=But#but.sources}|Acc] end, [], Buttons)}.

% GENERIC MATCH

fieldType(#field{type=empty}=X,Acc,Object,Opt) ->
    [#panel{class=box, style="display:none;",id=atom([X#field.id,type(Object)])}|Acc];

fieldType(#field{type=dynamic}=X,Acc,Object,Opt) ->
    [X#field.raw|Acc];

fieldType(#field{type=comment}=X,Acc,Object,Opt) ->
    [#panel { id=atom([commentBlock,type(Object)]), class=box, body=[
         case X#field.tooltips of
              false -> [];
              true -> [ #panel { class=label, body="&nbsp;" },
                        #panel { class=field, body="&nbsp;" },
                        #panel { class=tool, body=[
                         #link { id=atom([commentlink,X#field.id]),
                                 class=tooltips,
                                 onclick=nitro:f("showComment(this);"),
                                 body=[#image{src="/app/img/icon-comment-blue2.png"} ]} ]}] end,
                        #panel { class=comment,
                                 id=atom([X#field.id,type(Object)]),
                                 body=[X#field.desc],
                                 style= case X#field.tooltips of
                                             false -> "";
                                             true  -> "display:none;" end} ]}|Acc];

fieldType(#field{type=card}=X,Acc,Object,Opt) ->
   [#panel { id=atom([X#field.id,type(Object)]), class=[box,pad0], body=[
             #panel { class=label, body = X#field.title},
             #panel { class=field, style="width:66.63%", body=
                       #panel { id=atom([X#field.id,1]), body=[
                       #panel { class=field,style="width:90%;", body =
                      #select { id=atom([X#field.id,2]), disabled=true,
                                validation=val(Opt,"Validation.card(e)"),
                                body= #option{selected=true, body= translate(loading)}}},
                       #panel { class=tool, body= [#image{src="/app/img/preloader.gif"}]} ]}} ]}|Acc];

fieldType(#field{type=bool}=X,Acc,Object,Opt) ->
  Options = [ #opt{name = [], title =  <<>>, checked = true},
              #opt{name = true, title =  <<"Так"/utf8>>},
              #opt{name = false, title = <<"Ні"/utf8>>} ],
  fieldType(X#field{type=select, options=Options},Acc,Object,Opt);

fieldType(#field{}=X,Acc,Object,Opt) ->
   Panel = case X#field.id of [] -> #panel{};
                                 _ -> #panel{id=atom([wrap,X#field.id])} end,
   Tooltips =
   [ case Tx of
          {N} -> #panel{ id=atom([tooltip,N]), body=[]};
          _ -> #link { class=tooltips,
                       tabindex="-1",
                       onmouseover="setHeight(this);",
                       body=[ #image { src= "app/img/question.png" },
                              #span  { body=Tx } ]} end || Tx <- lists:reverse(X#field.tooltips) ],

   Options =
   [ case {X#field.type, O#opt.noRadioButton} of
          {_,true}     -> #label{id=atom([label,O#opt.name]),
                                 body=[]};

          % SELECT/OPTION

          {select,_}   -> #option{value = O#opt.name,
                                  body = O#opt.title,
                                  selected = O#opt.checked};

          % COMBO/RADIO

          {combo,_}    -> #panel{body= #label{body= #radio{name=atom([X#field.id,type(Object)]),
                                                           id=atom([X#field.id,type(Object),O#opt.name]),
                                                           body = O#opt.title,
                                                           checked=O#opt.checked,
                                                           disabled=O#opt.disabled,
                                                           value=O#opt.name,
                                                           postback={X#field.id,type(Object),O#opt.name}}}};

          % CHECKBOX

          {check,_}  ->  #checkbox{id=atom([O#opt.name,type(Object)]),
                                   source=[atom([O#opt.name])],
                                   body = O#opt.title,
                                   disabled=O#opt.disabled,
                                   checked=O#opt.checked,
                                   postback={O#opt.name}} end || O <- X#field.options ],


%   io:format("O: ~p~n",[{X,Object}]),

   [ Panel#panel { class=X#field.boxClass, body=[
          #panel { class=X#field.labelClass, body = X#field.title},
          #panel { class=X#field.fieldClass, body = fieldType(X#field.type,X,Options,Object,Opt)},
          #panel { class=tool, body= case Tooltips of
                   [] -> [];
                    _ -> case length(Tooltips) of
                              1 -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                      lists:duplicate(length(Tooltips),#br{}),Tooltips)));
                              _ -> tl(lists:flatten(lists:zipwith(fun(A2,B2) -> [A2,B2] end,
                                      lists:duplicate(length(Tooltips),#panel{style="height:10px;"}),
                                      Tooltips))) end end} ]}|Acc].

% SECOND LEVEL MATCH

fieldType(text,X,Options,Object,Opt) ->
    #panel{id=atom([X#field.id,type(Object)]), body=X#field.desc};

fieldType(integer,X,Options,Object,Opt) ->
    nitro:f(X#field.format,
           [ case X#field.postfun of
                  [] -> pos(Object,X);
                  PostFun -> PostFun(pos(Object,X)) end] );

fieldType(money,X,Options,Object,Opt) ->
 [ #input{ id=atom([X#field.id,type(Object)]), pattern="[0-9]*",
           validation=val(Opt,nitro:f("Validation.money(e, ~w, ~w, '~s')",[X#field.min,X#field.max, translate({?MODULE, error})])),
           onkeyup="beautiful_numbers(event);",
           value=nitro:to_list(extract(Object,X)) },
           #panel{ class=pt10,body= [ translate({?MODULE, warning}),
                                      nitro:to_binary(X#field.min), " ", X#field.curr ] } ];

fieldType(pay,X,Options,Object,Opt) ->
   #panel{body=[#input{ id=atom([X#field.id,type(Object)]), pattern="[0-9]*",
                        validation=val(Opt,X#field.validation),
                        onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",
                           [X#field.length,X#field.type]),
                        onkeyup="beautiful_numbers(event);",
                        value=nitro:to_list(extract(Object,X)) }, <<" ">>,
                 #span{ body=X#field.curr}]};

fieldType(combo,X,Options,Object,Opt) ->
   case length(Options) of
        1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#br{}),Options)));
        _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#panel{}),Options))) end;

fieldType(select,X,Options,Object,Opt) ->
   #select{ id=atom([X#field.id,type(Object)]), postback=X#field.postback, body=Options};

fieldType(check,X,Options,Object,Opt) ->
   case length(Options) of
        1 -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#br{}),Options)));
        _ -> tl(lists:flatten(lists:zipwith(fun(A,B) -> [A,B] end,
                lists:duplicate(length(Options),#panel{}),Options))) end;

fieldType(string,X,Options,Object,Opt) ->
   #input{ class=column,
           id=atom([X#field.id,type(Object)]),
           disabled = X#field.disabled,
           validation=val(Opt,nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max])),
           value=extract(Object,X)};

fieldType(phone,X,Options,Object,Opt) ->
   #input{ id=atom([X#field.id,type(Object)]),
           class=phone,
           pattern="[0-9]*",
           onkeypress=nitro:f("return fieldsFilter(event, ~w, '~w');",[X#field.length,X#field.type]),
           validation=val(Opt,nitro:f("Validation.phone(e, ~w, ~w)",[X#field.min,X#field.max])),
           value=extract(Object,X)};

fieldType(auth,X,Options,Object,Opt) ->
 [ #input{ id=atom([X#field.id,type(Object)]),
           class=phone,
           type=password,
           onkeypress="return removeAllErrorsFromInput(this);",
           onkeyup="nextByEnter(event);",
           validation=val(Opt,nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max])),
           placeholder= translate({auth, holder}) },
    #span{ class=auth_link,
           body = [ #link{ href= translate({auth, lost_pass_link}),
                           target="_blank", postback=undefined,
                           body= translate({auth, lost_pass}) }, #br{},
                    #link{ href= translate({auth, change_login_link}),
                           target="_blank", postback=undefined,
                           body= translate({auth, change_login}) }]} ];

fieldType(otp,X,Options,Object,Opt) ->
   #input{ class=[phone,pass],
           type=password,
           id=atom([X#field.id,type(Object)]),
           placeholder="(XXXX)",
           pattern="[0-9]*",
           validation=val(Opt,"Validation.nums(e, 4, 4, \"otp\")")
         };

fieldType(comboLookup,X,Options,Object,Opt) ->
  #comboLookup{id=atom([X#field.id,type(Object)]),
               disabled = X#field.disabled,
               validation=val(Opt,nitro:f("Validation.length(e, ~w, ~w)",[X#field.min,X#field.max])),
               feed=X#field.bind,
               reader=[],
               chunk=20};

fieldType(file,X,Options,Object,Opt) -> [];

fieldType(calendar,X,Options,Object,Opt) ->
   #panel{class=[field],
          body=[#calendar{value = extract(Object,X),
                           id=atom([X#field.id,type(Object)]),
                           disabled = X#field.disabled,
                           onkeypress="return removeAllErrorsFromInput(this);",
                           validation=val(Opt,"Validation.calendar(e)"),
                           disableDayFn="disableDays4Charge",
                           class = ['input-date'],
                           minDate=X#field.min,
                           maxDate = X#field.max,
                           lang=ua}]}.

val(Options,Validation) ->
   case proplists:get_value(search,Options,false) of
        true -> [];
        false -> Validation end.
