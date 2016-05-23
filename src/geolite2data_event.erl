%% -*- mode: erlang; tab-width: 4; indent-tabs-mode: 1; st-rulers: [70] -*-
%% vim: ts=4 sw=4 ft=erlang noet
%%%-------------------------------------------------------------------
%%% @author Andrew Bennett <andrew@pixid.com>
%%% @copyright 2016, Andrew Bennett
%%% @doc
%%%
%%% @end
%%% Created :  13 May 2016 by Andrew Bennett <andrew@pixid.com>
%%%-------------------------------------------------------------------
-module(geolite2data_event).

%% API
-export([manager/0]).
-export([add_handler/2]).
-export([delete_handler/2]).
-export([refresh/1]).
-export([announce/2]).

%% Macros
-define(MANAGER, geolite2data_manager).

%%%===================================================================
%%% API functions
%%%===================================================================

manager() ->
	?MANAGER.

add_handler(Handler, Pid) ->
	gen_event:add_handler(?MANAGER, Handler, Pid).

delete_handler(Handler, Pid) ->
	gen_event:delete_handler(?MANAGER, Handler, Pid).

refresh(Key) ->
	notify({database, refresh, Key}).

announce(Key, Filename) ->
	notify({database, announce, Key, Filename}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
notify(Message) ->
	gen_event:notify(?MANAGER, Message).
