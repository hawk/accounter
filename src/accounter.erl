%% Copyright 2015 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(accounter).
-export([
         get_work_dir/1, get_templates_dir/0, get_examples_dir/0,
         get_books_dir/1,get_book_name/1, get_latest_book_name/1, list_books/1,
         get_latest_voucher_id/1, get_latest_date/1,
         get_bindings/1, lookup_binding/4, get_var/3,
         pad_right/2, from_ore/1, from_ore/2
        ]).

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
    filename:join([get_work_dir(Args), "accounter.xml"]).

get_app_dir() ->
    {file, BeamPath} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(BeamPath)).

get_templates_dir() ->
    AppDir = get_app_dir(),
    filename:join([AppDir, "templates"]).

get_examples_dir() ->
    AppDir = get_app_dir(),
    filename:join([AppDir, "examples"]).

get_bindings(Args) ->
    ConfigFile = get_config_file(Args),
    Config = accounter_xml:parse_simple(ConfigFile),
    FileContext = [ConfigFile, file],
    Accounter = ?XML_LOOKUP(accounter, [Config], FileContext),
    AccounterContext = [accounter | FileContext],
    Template = ?XML_LOOKUP(template, Accounter, FileContext),
    TemplatesDir = get_templates_dir(),
    BindingsFile = filename:join([TemplatesDir, Template, "bindings.xml"]),
    Bindings0 = [accounter_xml:parse_simple(BindingsFile)],
    TemplateContext = [BindingsFile, file],
    Bindings = ?XML_LOOKUP(bindings, Bindings0, TemplateContext),
    BindingsContext = [bindings | TemplateContext],
    User = [format_binding(B, BindingsContext, ?FILE, ?LINE) || B <- Bindings],
    Builtin = gen_bindings(Args, Accounter, AccounterContext),
    Builtin ++ User.

gen_bindings(Args, Accounter, AccounterContext) ->
    Latest = get_latest_book_name(Args),
    Name = get_book_name(Args, Latest),
    Association = ?XML_LOOKUP(association, Accounter, AccounterContext),
    LogoRef = ?XML_LOOKUP(logo_ref, Accounter, AccounterContext),
    LogoIcon = ?XML_LOOKUP(logo_icon, Accounter, AccounterContext),
    [
     {"BOOK_LATEST", Latest},
     {"BOOK_NAME", Name},
     {"BOOK_REF", "?name="++Name},
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
       Tuple when is_tuple(Tuple) ->
           %% Multiple values. Return first.
           element(1, Tuple);
        undefined when is_function(Default) ->
            Default();
        undefined ->
            Default
    end.

get_book_name(Args) ->
    DefaultFun = fun() -> get_latest_book_name(Args) end,
    get_book_name(Args, DefaultFun).

get_book_name(Args, Default) ->
    get_var(Args, name, Default).

get_latest_book_name(Args) ->
    BooksDir = get_books_dir(Args),
    case list_books(BooksDir) of
        {ok, RevSortedNames} ->
            hd(RevSortedNames);
        {error, _Reason} ->
            {Year, _Month, _Day} = erlang:date(),
            integer_to_list(Year)
    end.

list_books(BooksDir) ->
    case file:list_dir(BooksDir) of
        {ok, []} ->
            {error, file:format_error(enoent)};
        {ok, Names} ->
            Rev = fun(X, Y) -> X > Y end,
            SortedNames =
                [Name || Name <- lists:sort(Rev, Names),
                         is_dir(BooksDir, Name)],
            {ok, SortedNames};
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

get_latest_voucher_id(#book{vouchers = []}) ->
    0;
get_latest_voucher_id(#book{vouchers = Vouchers}) ->
    lists:max([V#voucher.id || V <- Vouchers]).

get_latest_date(#book{name = Name, vouchers = []}) ->
    Name ++ "-01-01"; % First January
get_latest_date(#book{vouchers = Vouchers}) ->
    V = lists:last(Vouchers),
    V#voucher.date.

is_dir(BooksDir, Name) ->
    DirName = filename:join([BooksDir, Name]),
    filelib:is_dir(DirName).

%%-------------------------------------------------------------------
%% Paddings

pad_right(Int, N) ->
    string:right(integer_to_list(Int), N, $0).

from_ore(Ore, Delim) ->
    if
        Ore > 0 -> % Debit
            [from_ore(Ore), Delim, from_ore(0)];
        Ore < 0 -> % Credit
            [from_ore(0), Delim, from_ore(-Ore)];
        Ore =:=  0 ->
            [from_ore(0), Delim, from_ore(0)]
    end.

from_ore(Ore) ->
    [integer_to_list(Ore div 100), $,,
     accounter:pad_right(Ore rem 100, 2), " kr"].
