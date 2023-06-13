-module(heredocs).

-compile([nowarn_export_all, export_all]).

-include_lib("eunit/include/eunit.hrl").

heredocs([$",$",$" | Cs0]) ->
    case trim(Cs0) of
        {ok, Cs} ->
            do_heredocs(Cs, push, []);
        error ->
            error(badarg)
    end;
heredocs([_|Cs]) ->
    heredocs(Cs);
heredocs([]) ->
    error(eof).

do_heredocs([$",$",$"|_], {indent, _} = I, Acc) ->
    {Result, _} =
        lists:foldl(
            fun
            ($\n, {[$\\,$\\ | Acc1], _}) ->
                {[$\n,$\\|Acc1], I};
            ($\n, {[$\\ | Acc1], _}) ->
                {Acc1, I};
            ($\n, {Acc1, _}) ->
                {[$\n|Acc1], I};
            (C, {Acc1, {indent, 0}}) ->
                {[C|Acc1], push};
            (32, {Acc1, {indent, Cnt}}) ->
                {Acc1, {indent, Cnt - 1}};
            (_, {_, {indent, _}}) ->
                % Elixir's shows a warning instead of an error:
                % iex(1)>   """
                % ...(1)> foo
                % ...(1)>   """
                % warning: outdented heredoc line. The contents inside the heredoc should be indented at the same level as the closing """. The following is forbidden:
                %
                %     def text do
                %       """
                %     contents
                %       """
                %     end
                %
                % Instead make sure the contents are indented as much as the heredoc closing:
                %
                %     def text do
                %       """
                %       contents
                %       """
                %     end
                %
                % The current heredoc line is indented too little
                %   iex:3:3
                %
                % "foo\n"
                error(outdented);
            (C, {Acc1, _}) ->
                {[C|Acc1], push}
            end,
            {[], I},
            lists:reverse(Acc)
        ),
    lists:reverse(Result);
do_heredocs([$\n|Cs], skip, Acc) ->
    do_heredocs(Cs, {indent, 0}, Acc);
do_heredocs([$\n|Cs], _, Acc) ->
    do_heredocs(Cs, {indent, 0}, [$\n|Acc]);
do_heredocs([32|Cs], {indent, I}, Acc) ->
    do_heredocs(Cs, {indent, I+1}, [32|Acc]);
do_heredocs([$\\,$%|Cs], push, Acc) ->
    do_heredocs(Cs, skip, Acc);
do_heredocs([_|Cs], skip, Acc) ->
    do_heredocs(Cs, skip, Acc);
do_heredocs([C|Cs], _, Acc) ->
    do_heredocs(Cs, push, [C|Acc]);
do_heredocs([], _, _) ->
    error.

trim([32|Cs]) ->
    trim(Cs);
trim([$\n|Cs]) ->
    {ok, Cs};
trim(_) ->
    % Elixir's example of this error:
    % iex(1)> """foo
    % ** (SyntaxError) iex:1:1: heredoc allows only zero or more whitespace characters followed by a new line after """
    %   |
    % 1 | """foo
    %   | ^
    error(badarg).

-define(aeq(Expected, Filename),
    ?assertEqual(Expected, heredocs(get_file_content(Filename)))
).

-define(araise(Term, Filename),
    ?assertException(error, Term, heredocs(get_file_content(Filename)))
).

get_file_content(Filename) ->
    Dir = code:priv_dir(heredocs),
    {ok, Content} = file:read_file(filename:join([Dir, Filename])),
    binary_to_list(Content).

heredocs_test() ->
    [
        ?aeq("    this\n    is\n    a\n    test\n", "elixir_example_1"),
        ?aeq("This\nIs\nA\nTest\n", "elixir_example_2"),
        ?aeq("this is a\nvery long\nstring\n", "valim_example_1"),
        ?aeq("  this is a\n  very long\n  string\n", "valim_example_2"),
        ?aeq("this is a very long string", "valim_no_line_break"),
        ?aeq("this is a \\\nvery long \\\nstring\\\n", "valim_line_break"),
        ?aeq("This is an example triple-quoted string.\n  Even with indents.\n", "jchrist_example_1"),
        ?aeq("  This is an example indented triple-quoted-string.\n", "jchrist_example_2"),
        ?aeq("This is an example for how to \"naturally\" de-dent it in Python.\n", "jchrist_example_3"),
        ?aeq("this should contains \"quotes\"\nand \"\"\"triple quotes\"\"\" and\nends here\n", "william_test_quotes"),
        ?aeq("foo bar", "william_test_comment"),
        ?araise(badarg, "william_test_badarg"),
        ?araise(outdented, "william_test_outdented")
    ].
