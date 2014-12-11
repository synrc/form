-include_lib("kvs/include/kvs.hrl").

-ifndef(META_HRL).
-define(META_HRL, "meta_hrl").

-record(document,   { ?ITERATOR(feed), name, base, sections, fields, buttons, access }).
-record(validation, { name, msg, options=[], function }).
-record(sec,        { id, name, desc }).
-record(but,        { id, postback, name, title, sources=[], class }).
-record(opt,        { id, name, title, postback, checked=false }).
-record(sel,        { id, name, title, postback }).
-record(field,      { id, sec=1, name, pos, title, layout, visible=true, format="~w",
                      postfun=fun(X) -> X end, desc, wide=normal, type=binary, etc,
                      access, tooltip, options, min=0, max=10 }).

-endif.
