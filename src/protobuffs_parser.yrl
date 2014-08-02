Nonterminals
g_protobuffs g_message g_element g_elements g_var g_value g_default g_cmd g_module.

Terminals ';' '=' '{' '}' '[' ']' '<' '>' string integer float var.

Rootsymbol g_protobuffs.
Endsymbol '$end'.

g_protobuffs -> '$empty'					: [].
g_protobuffs -> g_message g_protobuffs 				: ['$1'|'$2'].

g_message -> g_var g_var g_cmd g_module  '{' g_elements '}'			: {'$1', safe_string('$2'), '$3', '$4', '$6'}.
g_message -> g_var g_var g_cmd g_module  '{' '}'					: {'$1', safe_string('$2'), '$3', '$4',  []}.

g_elements -> g_element						: ['$1'].
g_elements -> g_element g_elements				: ['$1' | '$2'].

g_element -> g_var g_var g_var '=' integer g_default ';'	: {unwrap('$5'), '$1', safe_string('$2'), safe_string('$3'), default('$1','$6')}.

g_var -> var 							: unwrap('$1').

g_value -> g_var						: '$1'.
g_value -> integer						: unwrap('$1').
g_value -> string						: unwrap('$1').
g_value -> float						: unwrap('$1').

g_default -> '$empty' : none.
g_default -> '[' g_var '=' g_value ']' 				: {'$2', '$4'}.

g_cmd -> '$empty' : none.
g_cmd -> '<' integer '>'				:$2.

g_module -> '$empty' : none.
g_module -> '[' g_var ']'				:$2.

Erlang code.
safe_string(A) -> make_safe(atom_to_list(A)).

make_safe(String) ->
  case erl_scan:reserved_word(list_to_atom(String)) of 
    true -> "pb_"++String;
    false -> String
  end.

unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.

default(repeated, _) ->
  [];
default(_, {default,D}) ->
  D;
default(_, _) ->
  none.
