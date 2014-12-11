-ifndef(META_HRL).
-define(META_HRL, "meta_hrl").

-include_lib("kvs/include/kvs.hrl").

-record(document,   { ?ITERATOR(feed), name, base, sections, fields, buttons, access }).
-record(validation, { name, msg, options=[], function }).
-record(sec,        { id, name, desc="" }).
-record(but,        { id, postback, name, title, sources=[], class }).
-record(opt,        { id, name, title, postback, checked=false }).
-record(sel,        { id, name, title, postback }).
-record(field,      { id, sec=1, name, pos, title, layout, visible=true, disabled=false, format="~w", curr="",
                      postfun=[], desc, wide=normal, type=binary, etc, labelClass=label, fieldClass=field,
                      access, tooltips=[], options=[], min=0, max=1000000,length=10 }).

-endif.
