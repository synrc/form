-author("G-Grand").

-ifndef(STEP_WIZARD_HRL).
-define(STEP_WIZARD_HRL, "step_wizard").

-record(step_wizard, {
    current_step=1,
    steps = 1
}).

-endif.