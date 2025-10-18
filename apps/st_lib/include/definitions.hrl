-ifndef(__st_lib_definitions__).
-define(__st_lib_definitions__, true).

-type raw_definition() :: term().
-record(st_lib_definition, {module :: module(), value :: raw_definition()}).

-define(DEFINITION_REF(ID), {'$ref', ID}).
-define(DEFINITION(Mod, Value), #st_lib_definition{module = Mod, value = Value}).

-endif.
