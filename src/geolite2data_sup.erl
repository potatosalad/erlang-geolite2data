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
-module(geolite2data_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
	geolite2data = ets:new(geolite2data, [
		named_table,
		ordered_set,
		public,
		{read_concurrency, true}
	]),
	GeolixUpdaterChildSpec = case geolite2data:geolix_updater() of
		true ->
			?CHILD(geolite2data_geolix_updater, worker);
		false ->
			[]
	end,
	LoggerChildSpec = case geolite2data:logger() of
		true ->
			?CHILD(geolite2data_logger, worker);
		false ->
			[]
	end,
	ChildSpecs = lists:flatten([
		{geolite2data_event:manager(),
			{gen_event, start_link, [{local, geolite2data_event:manager()}]},
			permanent, 5000, worker, [gen_event]},
		LoggerChildSpec,
		GeolixUpdaterChildSpec,
		?CHILD(geolite2data_server, worker),
		?CHILD(geolite2data_database_sup, supervisor)
	]),
	Restart = {one_for_one, 10, 10},
	{ok, {Restart, ChildSpecs}}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
