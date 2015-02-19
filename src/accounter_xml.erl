%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_xml).
-export([parse_simple/1, lookup/5, missing/5]).

-include_lib("xmerl/include/xmerl.hrl").
-include("../include/accounter.hrl").

%%-------------------------------------------------------------------
%% Library functions
%%-------------------------------------------------------------------

parse_simple(File) ->
    try
        case xmerl_scan:file(File, [{quiet,true}]) of
            {error, ScanReason} ->
                exit({error,
                      [file:format_error(ScanReason),
                       {file,File}]});
            {Internal, _Rest} ->
                to_simple(Internal)
        end
    catch
        exit:{fatal, {Reason, {file,BadFile}, {line,L}, {col,C}}} ->
            exit({error,
                  [Reason, {file,BadFile}, {line,L}, {col,C}]});
        exit:{error, FileReason} ->
            exit({error,
                  [file:format_error(FileReason), {file,File}]})
    end.

to_simple(List) when is_list(List) ->
    Simple = fun(Elem, Acc) ->
                     case to_simple(Elem) of
                         []       -> Acc;
                         Stripped -> [Stripped | Acc]
                     end
             end,
    lists:reverse(lists:foldl(Simple, [], List));
to_simple(#xmlElement{name    = Tag,
                      content = Content}) ->
    {Tag, lists:flatten(to_simple(Content))};
to_simple(#xmlText{value = Text}) ->
    strip(Text);
to_simple(#xmlComment{value = _Text}) ->
    "".

%% Strips off leading and trailing white spaces
strip([]) ->
    [];
strip([Char | Text]) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text);
strip(Text) ->
    strip(Text,[],[]).
strip([Char | Text], TAcc, SAcc) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text, TAcc, [Char | SAcc]);
strip([Char |Text], TAcc, SAcc) ->
    strip(Text, [Char | SAcc ++ TAcc], []);
strip([], TAcc, _SAcc) ->
    lists:reverse(TAcc).

lookup(Tag, List, Context, File, Line) ->
    case lists:keyfind(Tag, 1, List) of
        {Tag, Val} ->
            Val;
        false ->
            missing(Tag, List, Context, File, Line)
    end.

missing(Tag, List, Context, File, Line) ->
    exit({error,
          [missing_xml_tag,
           [Tag | Context],
           List,
           {file,File}, {line,Line}]}).
