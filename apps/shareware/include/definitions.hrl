-ifndef(__shareware_definitions__).
-define(__shareware_definitions__, true).

-type raw_definition() :: term().
-record(shareware_definition, {
    module :: module(),
    value :: raw_definition()
}).

-define(DEFINITION_REF(ID), {'$ref', ID}).
-define(DEFINITION(Mod, Value), #shareware_definition{
    module = Mod,
    value = Value
}).

-endif.
