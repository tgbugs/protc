#lang brag

doc        : (/ws* expression /ws*)*
expression : compound | process
process    : triple | mod | pair | single
/compound  : /lp operator (/ws* process /ws*)+ /rp
operator   : DUAL | CHAIN
triple     : material aspect material
mod        : INPUT-OUTPUT aspect OUTPUT-PRIME
pair       : material aspect | aspect material
single     : aspect
@material  : UNSPECIFIED | INPUT-OUTPUT
@aspect    : BAR | MEASURE | ACTUALIZE
@ws        : WS | COMMENT
@lp        : (/ws* L-PAREN /ws*)
@rp        : (/ws* R-PAREN /ws*)
