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
-module(geolite2data_hash).

%% API
-export([file/2]).
-export([file/3]).

%% Macros
-define(BLOCKSIZE, 32768).

%%%===================================================================
%%% API functions
%%%===================================================================

file(Type, Filename) ->
	file(Type, Filename, []).

file(Type, Filename, Options) ->
	case lists:delete(compressed, Options) of
		Options ->
			case file:open(Filename, [binary, raw, read, read_ahead | Options]) of
				{ok, IoDevice} ->
					read(IoDevice, crypto:hash_init(Type));
				OpenError ->
					OpenError
			end;
		OptionsWithoutCompression ->
			case file:open(Filename, [binary, raw, read, read_ahead | OptionsWithoutCompression]) of
				{ok, IoDevice} ->
					Z = zlib:open(),
					ok = zlib:inflateInit(Z, 31),
					read_compressed(IoDevice, crypto:hash_init(Type), Z);
				OpenError ->
					OpenError
			end
	end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
read(IoDevice, Context) ->
	case file:read(IoDevice, ?BLOCKSIZE) of
		{ok, Data} ->
			read(IoDevice, crypto:hash_update(Context, Data));
		eof ->
			_ = file:close(IoDevice),
			{ok, crypto:hash_final(Context)}
	end.

%% @private
read_compressed(IoDevice, Context, Z) ->
	case file:read(IoDevice, ?BLOCKSIZE) of
		{ok, Data} ->
			case zlib:inflateChunk(Z, Data) of
				{more, Decompressed} ->
					read_inflate(IoDevice, crypto:hash_update(Context, Decompressed), Z);
				Decompressed ->
					read_compressed(IoDevice, crypto:hash_update(Context, Decompressed), Z)
			end;
		eof ->
			ok = zlib:inflateEnd(Z),
			ok = zlib:close(Z),
			_ = file:close(IoDevice),
			{ok, crypto:hash_final(Context)}
	end.

%% @private
read_inflate(IoDevice, Context, Z) ->
	case zlib:inflateChunk(Z) of
		{more, Decompressed} ->
			read_inflate(IoDevice, crypto:hash_update(Context, Decompressed), Z);
		Decompressed ->
			read_compressed(IoDevice, crypto:hash_update(Context, Decompressed), Z)
	end.
