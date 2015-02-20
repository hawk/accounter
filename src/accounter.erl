%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter).
-export([
         get_work_dir/1,
         get_books_dir/1,get_book_name/1,
         get_bindings/1, lookup_binding/4,
         get_var/3, forward_query/2,
         to_html/1, pad_right/2
        ]).

-include_lib("kernel/include/file.hrl").
-include("../lib/yaws/include/yaws.hrl").
-include("../lib/yaws/include/yaws_api.hrl").
-include("../include/accounter.hrl").

%%-------------------------------------------------------------------
%% Library functions
%%-------------------------------------------------------------------

get_work_dir(Args) ->
    Error = fun() -> exit(no_work_dir) end,
    get_config(Args, work_dir, Error).

get_books_dir(Args) ->
    filename:join([get_work_dir(Args), "books"]).

get_config_file(Args) ->
    filename:join([get_work_dir(Args), "config.xml"]).

get_bindings(Args) ->
    ConfigFile = get_config_file(Args),
    Config = accounter_xml:parse_simple(ConfigFile),
    FileContext = [ConfigFile, file],
    Accounter = ?XML_LOOKUP(accounter, [Config], FileContext),
    AccounterContext = [accounter | FileContext],
    Bindings = ?XML_LOOKUP(bindings, Accounter, AccounterContext),
    BindingsContext = [bindings | AccounterContext],
    Read = [format_binding(B, BindingsContext, ?FILE, ?LINE) || B <- Bindings],
    Gen = gen_bindings(Accounter, AccounterContext),
    Read ++ Gen.

gen_bindings(Accounter, AccounterContext) ->
    Association = ?XML_LOOKUP(association, Accounter, AccounterContext),
    LogoRef = ?XML_LOOKUP(logo_ref, Accounter, AccounterContext),
    LogoIcon = ?XML_LOOKUP(logo_icon, Accounter, AccounterContext),
    [
     {"ASSOCIATION", Association},
     {"LOGO_REF", LogoRef},
     {"LOGO_ICON", LogoIcon}
    ].

format_binding({binding, Binding}, BindingsContext, _, _) ->
    BindingContext = [binding | BindingsContext],
    Replace = ?XML_LOOKUP(replace, Binding, BindingContext),
    With = ?XML_LOOKUP(with, Binding, BindingContext),
    {Replace, With};
format_binding([], _BindingsContext, _, _) ->
    [];
format_binding(Bindings, BindingsContext, File, Line) ->
    accounter_xml:missing(binding, Bindings, BindingsContext, File, Line).

lookup_binding(Replace, Bindings, File, Line) ->
    Error =
        fun() ->
                exit({error,
                      [missing_binding,
                       Replace,
                       Bindings,
                       {file,File}, {line,Line}]})
        end,
    case lookup(Replace, Bindings, Error) of
        "" ->
            exit({error,
                  [empty_binding,
                   Replace,
                   Bindings,
                   {file,File}, {line,Line}]});
        With ->
            With
    end.

get_config(#arg{opaque = Opaque}, Key, Default) ->
    %% io:format("Opaque: ~p\n", [Opaque]),
    lookup(Key, Opaque, Default).

lookup(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false when is_function(Default) ->
            Default();
        false ->
            Default;
        {Key, Val} ->
            Val
    end.

get_var(#arg{} = Args, Key, Default) ->
   case yaws_api:queryvar(Args, Key) of
        {ok, Val} ->
            Val;
        undefined when is_function(Default) ->
            Default();
        undefined ->
            Default
    end.

get_book_name(Args) ->
    Fun = fun() ->
                  BooksDir = get_books_dir(Args),
                  case list_books(BooksDir) of
                      {ok , [Name | _]} ->
                    Name;
                      {error, _Reason} ->
                          integer_to_list(element(1, erlang:date()))
                  end
          end,
    get_var(Args, name, Fun).

list_books(BooksDir) ->
    case file:list_dir(BooksDir) of
        {ok, Names} ->
            Rev = fun(X, Y) -> X > Y end,
            Names2 = [Name || Name <- lists:sort(Rev, Names),
                              is_dir(BooksDir, Name)],
            {ok, Names2};
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

is_dir(BooksDir, Name) ->
    DirName = filename:join([BooksDir, Name]),
    case file:read_file_info(DirName) of
        {ok, FI} when FI#file_info.type =:= directory ->
            true;
        _ ->
            false
    end.

forward_query(NewQuery, Args) ->
    OldQuery = [{Key, Val} || {Key, Val} <- yaws_api:parse_query(Args),
                              not lists:keymember(Key, 1, NewQuery)],
    to_query(NewQuery ++ OldQuery).

to_query([]) ->
    [];
to_query(Query) ->
    ["?", to_query2(Query)].

to_query2([{Key, Val} | Tail]) when is_atom(Key) ->
    to_query2([{atom_to_list(Key), Val} | Tail]);
to_query2([{Key, Val} | Tail]) when is_list(Key), is_list(Val) ->
    case Tail of
        [] ->
            [Key, "=", Val];
        _ ->
            [Key, "=", Val, "&", to_query2(Tail)]
    end.

to_html({kr, Int}) when is_integer(Int) ->
    [integer_to_list(Int div 100), ",",
     pad_right(abs(Int) rem 100, 2),
     " kr"];
to_html(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_html({Year, Month, Day}) ->
    [pad_right(Year, 4), "-", pad_right(Month, 2), "-", pad_right(Day, 2)];
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

pad_right(Int, N) ->
    string:right(integer_to_list(Int), N, $0).
