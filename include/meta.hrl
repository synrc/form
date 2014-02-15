-ifndef(META_HRL).
-define(META_HRL, true).

-include_lib("kvs/include/kvs.hrl").

-record(meta,{?CONTAINER}).
-record(document,{?ITERATOR(meta),name,base,fields,access}).

-record(field,{ id,
                section,
                name,
                pos,
                title,
                layout,
                visible=true,
                desc,
                wide=normal,
                type=binary,
                etc,
                access}).

-endif.
