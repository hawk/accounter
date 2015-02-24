%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter_html).
-export([
         to_html/1
        ]).

to_html({kr, Int}) when is_integer(Int) ->
    [integer_to_list(Int div 100), ",",
     accounter:pad_right(abs(Int) rem 100, 2),
     " kr"];
to_html(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_html({Year, Month, Day}) ->
    [accounter:pad_right(Year, 4), "-",
     accounter:pad_right(Month, 2), "-",
     accounter:pad_right(Day, 2)];
%%to_html("") ->
%%    "&nbsp;";
to_html(String) when is_list(String) ->
    to_html_string(String);
to_html(Atom) when is_atom(Atom) ->
    case Atom of
        true    -> "1";
        false   -> "0"
    end.

to_html_string([195, Char | Tail]) ->
    case Char of
        165 -> ["&aring;" | to_html_string(Tail)]; % aa
        164 -> ["&auml;"  | to_html_string(Tail)]; % ae
        182 -> ["&ouml;"  | to_html_string(Tail)]; % oe

        133 -> ["&Aring;" | to_html_string(Tail)]; % AA
        132 -> ["&Auml;"  | to_html_string(Tail)]; % AE
        150 -> ["&Ouml;"  | to_html_string(Tail)]; % OE
        _   -> [195, Char | to_html_string(Tail)]
    end;
to_html_string([Char | Tail]) ->
    [Char | to_html_string(Tail)];
to_html_string([]) ->
    [].
