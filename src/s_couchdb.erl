%%-------------------------------------------------------------------
%% @author Steven Chan <contact@stevenchan.hk>
%% @doc 
%% @end
%%-------------------------------------------------------------------
-module(s_couchdb).
-include("include/s_test.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([create_db/1, delete_db/1]).
-export([create/3, update/4, get/2, delete/3]).

%%====================================================================
%% Tests
%%====================================================================

db_creation_deletion_test() ->
    inets:start(),
    Settings = [{domain, "127.0.0.1"}, {port, 5984}, {database, "s_couchdb_test"}],
    delete_db(Settings),
    ?assertEqual({error, no_exists},      delete_db(Settings)),
    ?assertEqual(ok,                      create_db(Settings)),
    ?assertEqual({error, already_exists}, create_db(Settings)),
    ?assertEqual(ok,                      delete_db(Settings)).

document_test() ->
    inets:start(),
    
    % setup
    Settings = [{domain, "127.0.0.1"}, {port, 5984}, {database, "s_couchdb_test"}],
    delete_db(Settings),
    create_db(Settings),
    
    % fixture
    Id  = "doc1",
    Doc = [{"title", "There is Nothing Left to Lose"}, {"artist", "Foo Fighters"}],
    Doc2 = [{"title", "There is Nothing Left to Lose"}, {"artist", "Foo Fighters"}, {"year", "1997"}],
    
    % test
    ?assertEqual({error, no_exists}, delete(Settings, Id, "revision")),
    ?assertEqual({error, no_exists}, get(Settings, Id)),
    
    % create
    {ok, Revision, Doc} = create(Settings, Id, Doc),
    ?assertEqual({error, already_exists}, create(Settings, Id, Doc)),
    
    % update
    {ok, NewRevision, Doc2} = update(Settings, Id, Doc2, Revision),
    ?assertEqual({error, revision}, update(Settings, Id, Doc2, Revision)),
    
    % get
    {ok, NewRevision, DocRetrieved} = get(Settings, Id),
    ?assertListEqual(Doc2, DocRetrieved),
    
    % delete
    ?assertEqual({error, revision}, delete(Settings, Id, Revision)),
    ?assertEqual(ok, delete(Settings, Id, NewRevision)),
    
    % cleanup
    delete_db(Settings).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @type settings() = proplist().
%%       [{domain, string()}, {port, integer()}, {database, string()}]
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @spec create_db(settings()) -> ok | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
create_db(Settings) ->
    case status_code(request(Settings, put)) of
        201 -> ok;
        412 -> {error, already_exists};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec delete_db(Settings) -> ok | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
delete_db(Settings) ->
    case status_code(request(Settings, delete)) of
        200 -> ok;
        404 -> {error, no_exists};
        409 -> {error, already_exists};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec create(settings(), Id::string(), Doc::proplists()) -> ok | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
create(Settings, Id, Doc) ->
    Result = request(Settings, put, Id, Doc),
    case status_code(Result) of
        201 -> {ok, revision(Result), Doc};
        404 -> {error, no_exists};
        409 -> {error, already_exists};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec update(settings(), Id::string(), Doc::proplists(), Revision::string()) -> ok | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
update(Settings, Id, Doc, Revision) ->
    Result = request(Settings, put, Id, [{"_rev", Revision}|Doc]),
    case status_code(Result) of
        201 -> {ok, revision(Result), Doc};
        409 -> {error, revision};
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @spec get(settings(), Id::string()) -> {ok, Revision::string(), Doc::proplists()} | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
get(Settings, Id) ->
    Result = request(Settings, get, Id),
    case status_code(Result) of
        200 -> {ok, revision(Result), doc(Result)};
        404 -> {error, no_exists}
    end.

%%--------------------------------------------------------------------
%% @spec delete(settings(), Id::string(), Revision::string()) -> ok | {error, Reason}
%% @doc 
%% @end
%%--------------------------------------------------------------------
delete(Settings, Id, Revision) ->
    Result = request(Settings, delete, Id, Revision),
    case status_code(Result) of
        200 -> ok;
        404 -> {error, no_exists};
        409 -> {error, revision}
    end.
    

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @spec url_prefix(settings()) -> string()
%% @doc 
%% @end
%%--------------------------------------------------------------------
url_prefix(Settings) ->
    {Domain, Port, Database} = parse_settings(Settings),
    lists:append(["http://", Domain, ":", Port, "/", Database, "/"]).

%%--------------------------------------------------------------------
%% @spec parse_settings(settings()) -> {Domain::string(), Port::string(), Database::string()}
%% @doc 
%% @end
%%--------------------------------------------------------------------
parse_settings(Settings) ->
    Domain   = proplists:get_value(domain, Settings),
    Port     = integer_to_list(proplists:get_value(port, Settings)),
    Database = proplists:get_value(database, Settings),
    {Domain, Port, Database}.

%%--------------------------------------------------------------------
%% @spec status_code(HttpRequestResult) -> ok | {error, connection}
%% @doc 
%% @end
%%--------------------------------------------------------------------
status_code(HttpRequestResult) ->
    case HttpRequestResult of
        {ok, {{_, StatusCode, _}, _, _}} ->
            StatusCode;
        _ ->
            {error, connection}
    end.

%%--------------------------------------------------------------------
%% @doc Request
%% @end
%%--------------------------------------------------------------------
request(Settings, Method) ->
    case Method of
        % put requires content type and body
        put -> http:request(Method, {url_prefix(Settings), [], [], []}, [], []);
        _   -> http:request(Method, {url_prefix(Settings), []}, [], [])
    end.

request(Settings, Method, Id) ->
    http:request(Method, {lists:append([url_prefix(Settings), Id]), []}, [], []).

request(Settings, delete, Id, Revision) ->
    http:request(delete, {lists:append([url_prefix(Settings), Id, "?rev=", Revision]), []}, [], []);

request(Settings, Method, Id, Doc) ->
    http:request(Method, {lists:append([url_prefix(Settings), Id]), [], 
        "application/json", doc_to_json(Doc)}, [], []).

%%--------------------------------------------------------------------
%% @spec doc_to_json(Doc::proplists) -> string()
%% @doc 
%% @end
%%--------------------------------------------------------------------
doc_to_json(Doc) ->
    KeyValues = [lists:append(["\"", Key, "\":\"", Value, "\""]) || {Key, Value} <- Doc],
    lists:append(["{", string:join(KeyValues, ","), "}"]).

%%--------------------------------------------------------------------
%% @spec revision(HttpRequestResult) -> string()
%% @doc 
%% @end
%%--------------------------------------------------------------------
revision(HttpRequestResult) ->
    {ok, {_, _, ResultBody}} =  HttpRequestResult,
    case proplists:get_value("rev", parse_body(ResultBody)) of
        undefined -> proplists:get_value("_rev", parse_body(ResultBody));
        Rev -> Rev
    end.

%%--------------------------------------------------------------------
%% @spec doc(HttpRequestResult) -> proplist()
%% @doc 
%% @end
%%--------------------------------------------------------------------
doc(HttpRequestResult) ->
    {ok, {{_, _, _}, _, ResultBody}} =  HttpRequestResult,
    proplists:delete("_id", proplists:delete("_rev", parse_body(ResultBody))).

%%--------------------------------------------------------------------
%% @spec parse_body(ResultBody) -> proplist()
%% @doc 
%% @end
%%--------------------------------------------------------------------
parse_body(ResultBody) ->
    {struct, Result} = mochijson:decode(ResultBody),
    Result.