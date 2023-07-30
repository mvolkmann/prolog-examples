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

handle(Error) :-
  format("received error ~w~n", Error).
  % format("context is ~w~n", Context),
  % fail.

demo :-
  catch(
    % The first argument specifies the goal to try.
    double(-3, D),

    % The second argument specifies the kinds of errors to handle.
    % Passing a variable as the first argument of `error`
    % allows it to catch any kind of error.
    % Pass `error(specific_error_type, _)` to only catch a specfic kind.
    error(domain_error(Domain, Value), Context),
    % error(Kind(Domain, Value), Context),
    % Error,

    % The third argument specifies what to do after the error is thrown.
    (
      format(
        "~w was passed ~w which is not in the domain ~w.~n",
        [Context, Value, Domain]
      ),
      % format("Error = ~w~n", [Error]),
      % fail
      D = 0
    )
  ),
  format("D = ~d~n", [D]).
