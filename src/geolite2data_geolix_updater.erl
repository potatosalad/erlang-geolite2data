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
			_ = load_database(Key, Filename),
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
detect_support(#{ adapter := Adapter }) ->
	Res0 =
		case code:ensure_loaded(elixir) of
			{module, elixir} ->
				case code:ensure_loaded('Elixir.Geolix') of
					{module, 'Elixir.Geolix'} ->
						continue;
					_ ->
						abort
				end;
			_ ->
				abort
		end,
	Res1 =
		case Res0 of
			continue ->
				case code:ensure_loaded(Adapter) of
					{module, Adapter} ->
						case erlang:function_exported('Elixir.Geolix', load_database, 1) of
							true ->
								load_database;
							false ->
								continue
						end;
					_ ->
						continue
				end;
			Other0 ->
				Other0
		end,
	Res2 =
		case Res1 of
			continue ->
				case erlang:function_exported('Elixir.Geolix', set_database, 2) of
					true ->
						set_database;
					false ->
						abort
				end;
			Other1 ->
				Other1
		end,
	Res2.

%% @private
load_database(Key, Filename) ->
	Source = to_string(Filename),
	Database = #{
		id => Key,
		adapter => 'Elixir.Geolix.Adapter.MMDB2',
		source => Source
	},
	case detect_support(Database) of
		load_database ->
			_ = application:ensure_all_started(geolix),
			'Elixir.Geolix':load_database(Database);
		set_database ->
			_ = application:ensure_all_started(geolix),
			'Elixir.Geolix':set_database(Key, Source);
		abort ->
			ok
	end.

%% @private
to_string(List) when is_list(List) ->
	binary:list_to_bin(List);
to_string(Binary) when is_binary(Binary) ->
	Binary.
