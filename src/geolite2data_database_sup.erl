%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2016, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  14 May 2016 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(geolite2data_database_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_database/3]).
-export([stop_database/1]).

%% Supervisor callbacks
-export([init/1]).

%% Macros
-define(SUPERVISOR, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

start_database(Key, DigestURL, CompressedURL) ->
	supervisor:start_child(?SUPERVISOR, geolite2data_database:child_spec(Key, DigestURL, CompressedURL)).

stop_database(Key) ->
	case supervisor:terminate_child(?SUPERVISOR, {geolite2data_database, Key}) of
		ok ->
			_ = supervisor:delete_child(?SUPERVISOR, {geolite2data_database, Key}),
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	ChildSpecs = child_specs(),
	Restart = {one_for_one, 10, 10},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
child_specs() ->
	[begin
		geolite2data_database:child_spec(Key, DigestURL, CompressedURL)
	end || {Key, DigestURL, CompressedURL} <- geolite2data:list_databases()].
