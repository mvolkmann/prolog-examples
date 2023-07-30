:- use_module(library(clpz)).

% This throws if N is less than zero.
double(N, D) :-
  ( N #>= 0 ->
    D #= N * 2
  ; throw(error(
      domain_error(non_negative_integer, N),
      double/2
    ))
  ).

demo :-
  catch(
    % The first argument specifies the goal to try.
    double(-3, D),

    % The second argument specifies the kinds of errors to handle.
    error(domain_error(Domain, Value), Context),

    % The second argument can also be a variable to catch any kind of error.
    % Error,

    % The third argument specifies what to do after an error is caught.
    (
      format(
        "~w was passed ~w which is not in the domain ~w.~n",
        [Context, Value, Domain]
      ),

      % Use this instead of the previous line
      % when the second argument is a variable.
      % format("Error = ~w~n", [Error]),

      % optionally fail this rule when an error is caught.
      % fail

      % This provides a value for D when an error is caught.
      D = 0
    )
  ),
  format("D = ~d~n", [D]).
