-ifndef(META_HRL).
-define(META_HRL, true).

-include_lib("kvs/include/kvs.hrl").

-record(meta,{?CONTAINER}).
-record(document,{?ITERATOR(meta),name,base,sections,fields,access}).

-record(section, {id,name,desc}).
-record(field,{ id,sec,name,pos,title,layout,visible=true,desc,wide=normal,
                type=binary,etc,access}).

-record(validation, {name, msg, options=[], function}).

-endif.
