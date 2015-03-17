-author("G-Grand").

-ifndef(STEP_WIZARD_HRL).
-define(STEP_WIZARD_HRL, "step_wizard").

-include_lib("kvs/include/kvs.hrl").

-record(step_wizard, {?ITERATOR(feed),
    current_step=undefined,
    steps = undefined
}).

-endif.