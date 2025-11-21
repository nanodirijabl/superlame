-ifndef(__stdm_definitions__).
-define(__stdm_definitions__, true).

-type raw_definition() :: term().
-record(stdm_definition, {module :: module(), value :: raw_definition()}).

-define(DEFINITION_REF(ID), {'$ref', ID}).
-define(DEFINITION(Mod, Value), #stdm_definition{module = Mod, value = Value}).

-endif.
