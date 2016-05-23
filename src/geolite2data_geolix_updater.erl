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
-module(geolite2data_geolix_updater).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Types
-record(state, {
	databases = #{} :: #{atom() => file:filename_all()}
}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
	ok = geolite2data_event:add_handler(geolite2data_event_handler, self()),
	{ok, #state{}}.

%% @private
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({'$geolite2data-event', {database, announce, Key, Filename}}, State=#state{databases=Databases}) ->
	case maps:get(Key, Databases, undefined) of
		Filename ->
			{noreply, State};
		_ ->
			_ = set_database(Key, Filename),
			{noreply, State#state{databases=maps:put(Key, Filename, Databases)}}
	end;
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok = geolite2data_event:delete_handler(geolite2data_event_handler, self()),
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	ok = geolite2data_event:delete_handler(geolite2data_event_handler, self()),
	ok = geolite2data_event:add_handler(geolite2data_event_handler, self()),
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
set_database(Key, Filename) ->
	case code:ensure_loaded(elixir) of
		{module, elixir} ->
			case code:ensure_loaded('Elixir.Geolix') of
				{module, 'Elixir.Geolix'} ->
					_ = application:ensure_all_started(geolix),
					'Elixir.Geolix':set_database(Key, to_string(Filename));
				_ ->
					ok
			end;
		_ ->
			ok
	end.

%% @private
to_string(List) when is_list(List) ->
	binary:list_to_bin(List);
to_string(Binary) when is_binary(Binary) ->
	Binary.
