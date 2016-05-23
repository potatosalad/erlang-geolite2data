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
-module(geolite2data_app).
-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]).
-export([config_change/3]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_Type, _Args) ->
	geolite2data_sup:start_link().

stop(_State) ->
	ok.

config_change(_Changed, _New, _Removed) ->
	geolite2data_server:config_change().
