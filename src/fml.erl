%%%-------------------------------------------------------------------
%%% @author Zatolokin Pavel
%%% @copyright (C) 2015, PrivatBank
%%% @doc
%%%
%%% @end
%%% Created : 09. июн 2015 16:54
%%%-------------------------------------------------------------------
-module(fml).
-author('Zatolokin Pavel').

%% API
-export([new/2]).
-include_lib("n2o/include/wf.hrl").
-include_lib("step_wizard.hrl").
-include("meta.hrl").

new(Document,Object) ->
    StepWizard = Document#document.steps,
    [Section]  = Document#document.sections,
    Fields     = Document#document.fields,
    Name       = Document#document.name,
    Buttons    = Document#document.buttons,
    jsone:encode({[
        {<<"type">>, <<"col">>},
        {<<"bg">>, <<"#e9f5c9">>},
        {<<"color">>, <<"#000000">>},
        {<<"pd">>,[10]},
        {<<"items">>,lists:flatten([

%%          % step wizard
            case StepWizard of
                #step_wizard{} ->
                    [{[{<<"type">>,<<"text">>}, {<<"talign">>,<<"right">>},
                        {<<"val">>, wf:to_binary(wf:f(deposits:translate(step_wizard),
                            [StepWizard#step_wizard.current_step,StepWizard#step_wizard.steps])) }]},
                        {[{<<"type">>,<<"text">>}, {<<"val">>,<<"<hr/>">>}]}];
                _ -> {[]}
            end,

            % caption
            {[{<<"type">>,<<"text">>}, {<<"talign">>,<<"center">>}, {<<"mg">>,[5]}, {<<"val">>,<<"<h3>",(Section#sec.name)/binary,"</h3>">>}]},
            {[{<<"type">>,<<"text">>}, {<<"val">>, wf:to_binary(Section#sec.desc)}]},

            % fields
            [ {[{<<"type">>,<<"row">>}, {<<"mg">>,[10,0]}, {<<"items">>,[label(F), field(F,Name,Object)]}]} || F <- Fields],

            % buttons
            {[{<<"type">>,<<"row">>}, {<<"items">>,
                [ {[{<<"type">>,<<"button">>},
                    {<<"bg">>,<<"#64a433">>},
                    {<<"b_radius">>,4},
                    {<<"b_size">>,1},
                    {<<"b_color">>,<<"#bbb">>},
                    {<<"mg">>,[<<"2.5">>]},
                    {<<"title">>,But#but.title}%,
%%                     {<<"val">>,term_to_binary(But#but.postback)}
                   ]}
                || But <- Buttons, is_record(But,but)]
            }]}
        ])}
    ]}).

label(#field{type=empty}) -> {[]};
label(#field{type=comment}) -> {[]};
label(F) when F#field.type==money; F#field.type==string ->
    {[{<<"type">>,<<"text">>}, {<<"talign">>,<<"right">>}, {<<"valign">>,<<"center">>}, {<<"pd">>,[0,10]}, {<<"val">>,wf:to_binary(F#field.title)}]};
label(F) -> {[{<<"type">>,<<"text">>}, {<<"talign">>,<<"right">>}, {<<"pd">>,[0,10]}, {<<"val">>,wf:to_binary(F#field.title)}]}.


field(F = #field{type=text}, _,_) ->
    {[{<<"type">>,<<"text">>}, {<<"val">>,wf:to_binary(F#field.desc)}]};

field(F = #field{type=integer}, _,Object) ->
    Value = wf:to_binary(wf:f(F#field.format,[
        case F#field.postfun of
            [] -> element(F#field.pos,Object);
            PostFun -> PostFun(element(F#field.pos,Object))
        end] )),
    {[{<<"type">>,<<"text">>}, {<<"val">>,Value}]};

field(F = #field{type=string},_,Object) ->
    {[{<<"type">>,<<"edit">>}, {<<"maxlen">>, F#field.max}, {<<"val">>, wf:to_binary(element(F#field.pos,Object))}]};

field(F = #field{type=money},_,Object) ->
    {[{<<"type">>,<<"edit">>}, {<<"maxlen">>, F#field.length}, {<<"val">>, wf:to_binary(element(F#field.pos,Object))}]};

field(F = #field{type=combo},Name,_) ->
    Vars = [ {[{<<"v">>,wf:atom([O#opt.name,Name])}, {<<"t">>,O#opt.title}]} || O <- F#field.options],
    Checked = case lists:keyfind(true, #opt.checked, F#field.options) of
                  Opt = #opt{} -> wf:atom([Opt#opt.name,Name]);
                  _ -> <<>>
              end,
    {[{<<"type">>,<<"radio">>}, {<<"name">>,wf:atom([F#field.name,combo])}, {<<"val">>,Checked}, {<<"vars">>,Vars}]};

field(#field{type=card},_,_) ->
    Vars = [ {[{<<"v">>,<<"none">>}, {<<"t">>,deposits:translate({deposits,selectcard})}]} ],
    {[{<<"type">>,<<"select">>}, {<<"vars">>,Vars}]};

field(_,_,_) -> {[]}.
