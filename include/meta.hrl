-ifndef(FORMS_META_HRL).
-define(FORMS_META_HRL, "meta_hrl").

-include_lib("kvs/include/kvs.hrl").

-record(document,   { ?ITERATOR(feed), name=[], base=[], sections=[], fields=[], buttons=[], access=[], steps=[] }).
-record(validation, { name, type, msg, extract = fun(X) -> X end, options=[], function, field={record,pos} }).
-record(sec,        { id, name=[], desc=[], nameClass=[], descClass=small }).
-record(but,        { id, postback, name, title, sources=[], class }).
-record(opt,        { id, name, title, postback, checked=false, disabled=false, noRadioButton=false }).
-record(sel,        { id, name, title, postback }).
-record(field,      { id, sec=1, name, pos, title, layout, visible=true, disabled=false, format="~w", curr="",
                      postfun=[], desc, wide=normal, type=binary, etc, labelClass=label, fieldClass=field,
                      access, tooltips=[], options=[], min=0, max=1000000,length=10 }).

-endif.
