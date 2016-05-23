%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2016, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  28 Mar 2016 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(geolite2data).

%% API
-export([autoupdate/0]).
-export([autoupdate/1]).
-export([data_dir/0]).
-export([data_dir/1]).
-export([delete_database/1]).
-export([geolix_updater/0]).
-export([geolix_updater/1]).
-export([get_database/1]).
-export([get_database/2]).
-export([keep_releases/0]).
-export([keep_releases/1]).
-export([list_databases/0]).
-export([logger/0]).
-export([logger/1]).
-export([store_database/3]).
%% Private API
-export([start/0]).

%% Macros
-define(LOAD_APP(F), begin
	_ = application:load(geolite2data),
	F
end).
-define(MAYBE_START_APP(F), try
	F
catch
	_:_ ->
		_ = geolite2data:start(),
		F
end).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec autoupdate() -> boolean() | pos_integer().
autoupdate() ->
	?LOAD_APP(application:get_env(?MODULE, autoupdate, true)).

-spec autoupdate(BooleanOrSeconds::boolean() | pos_integer()) -> ok.
autoupdate(BooleanOrSeconds)
		when is_boolean(BooleanOrSeconds)
		orelse (is_integer(BooleanOrSeconds) andalso BooleanOrSeconds > 0) ->
	?LOAD_APP(application:set_env(?MODULE, autoupdate, BooleanOrSeconds)),
	?MAYBE_START_APP(geolite2data_server:config_change()).

data_dir() ->
	case ?LOAD_APP(application:get_env(?MODULE, data_dir)) of
		{ok, Directory} when is_binary(Directory) orelse is_list(Directory) ->
			Directory;
		_ ->
			priv_dir()
	end.

data_dir(Directory) when is_binary(Directory) orelse is_list(Directory) ->
	?LOAD_APP(application:set_env(?MODULE, data_dir, Directory)),
	?MAYBE_START_APP(geolite2data_server:config_change()).

delete_database(Key) when is_atom(Key) ->
	geolite2data_database_sup:stop_database(Key),
	Databases = lists:keydelete(Key, 1, list_databases()),
	application:set_env(?MODULE, databases, Databases),
	?MAYBE_START_APP(geolite2data_server:config_change()).

geolix_updater() ->
	?LOAD_APP(application:get_env(?MODULE, geolix_updater, false)).

geolix_updater(Boolean) when is_boolean(Boolean) ->
	?LOAD_APP(application:set_env(?MODULE, geolix_updater, Boolean)),
	_ = geolite2data:start(),
	case Boolean of
		true ->
			Started = supervisor:start_child(geolite2data_sup, {
				geolite2data_geolix_updater,
				{geolite2data_geolix_updater, start_link, []},
				permanent, 5000, worker, [geolite2data_geolix_updater]
			}),
			ok = case Started of
				{ok, _} ->
					ok;
				{error, already_present} ->
					ok;
				{error, {already_started, _}} ->
					ok
			end;
		false ->
			case supervisor:terminate_child(geolite2data_sup, geolite2data_geolix_updater) of
				ok ->
					_ = supervisor:delete_child(geolite2data_sup, geolite2data_geolix_updater),
					ok;
				{error, _Reason} ->
					ok
			end
	end,
	?MAYBE_START_APP(geolite2data_server:config_change()).

get_database(Key) when is_atom(Key) ->
	get_database(Key, 5000).

get_database(Key, Timeout) when is_atom(Key) ->
	?MAYBE_START_APP(geolite2data_database:get_database(Key, Timeout)).

keep_releases() ->
	?LOAD_APP(application:get_env(?MODULE, keep_releases, 2)).

keep_releases(Number) when is_integer(Number) andalso Number > 0 ->
	?LOAD_APP(application:set_env(?MODULE, keep_releases, Number)),
	?MAYBE_START_APP(geolite2data_server:config_change()).

list_databases() ->
	?LOAD_APP(application:get_env(?MODULE, databases, [])).

logger() ->
	?LOAD_APP(application:get_env(?MODULE, logger, false)).

logger(Boolean) when is_boolean(Boolean) ->
	?LOAD_APP(application:set_env(?MODULE, logger, Boolean)),
	_ = geolite2data:start(),
	case Boolean of
		true ->
			Started = supervisor:start_child(geolite2data_sup, {
				geolite2data_logger,
				{geolite2data_logger, start_link, []},
				permanent, 5000, worker, [geolite2data_logger]
			}),
			ok = case Started of
				{ok, _} ->
					ok;
				{error, already_present} ->
					ok;
				{error, {already_started, _}} ->
					ok
			end;
		false ->
			case supervisor:terminate_child(geolite2data_sup, geolite2data_logger) of
				ok ->
					_ = supervisor:delete_child(geolite2data_sup, geolite2data_logger),
					ok;
				{error, _Reason} ->
					ok
			end
	end,
	?MAYBE_START_APP(geolite2data_server:config_change()).

store_database(Key, DigestURL, CompressedURL)
		when is_atom(Key)
		andalso (is_binary(DigestURL) orelse is_list(DigestURL))
		andalso (is_binary(CompressedURL) orelse is_list(CompressedURL)) ->
	NewDatabase = {Key, to_char_list(DigestURL), to_char_list(CompressedURL)},
	OldDatabases = list_databases(),
	Databases = lists:keystore(Key, 1, OldDatabases, NewDatabase),
	application:set_env(?MODULE, databases, Databases),
	_ = geolite2data:start(),
	case lists:keymember(Key, 1, OldDatabases) of
		false ->
			ok;
		true ->
			geolite2data_database_sup:stop_database(Key)
	end,
	geolite2data_database_sup:start_database(Key, DigestURL, CompressedURL).

%%====================================================================
%% Private API functions
%%====================================================================

start() ->
	case application:ensure_all_started(?MODULE) of
		{ok, _} ->
			ok;
		StartError ->
			StartError
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
priv_dir() ->
	case code:priv_dir(?MODULE) of
		{error, bad_name} ->
			case code:which(?MODULE) of
				Filename when is_list(Filename) ->
					filename:join([filename:dirname(Filename), "../priv"]);
				_ ->
					"../priv"
			end;
		Dir ->
			Dir
	end.

%% @private
to_char_list(List) when is_list(List) ->
	List;
to_char_list(Binary) when is_binary(Binary) ->
	binary:bin_to_list(Binary).
