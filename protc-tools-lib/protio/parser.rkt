#lang brag

doc        : (/ws* expression /ws*)*
expression : triple | mod | pair | single
triple     : material aspect material
mod        : INPUT-OUTPUT aspect OUTPUT-PRIME
pair       : material aspect | aspect material
single     : aspect
@material  : UNSPECIFIED | INPUT-OUTPUT
@aspect    : BAR | MEASURE | ACTUALIZE
@ws        : WS | COMMENT
