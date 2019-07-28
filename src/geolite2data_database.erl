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
-module(geolite2data_database).
-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([child_spec/3]).
-export([get_database/2]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% Types
-record(state, {
	key             = undefined   :: undefined | atom(),
	digest_url      = undefined   :: undefined | iodata(),
	compressed_url  = undefined   :: undefined | iodata(),
	name            = idle        :: idle | poll | wait,
	last_checked    = 0           :: integer(),
	timer_reference = undefined   :: undefined | reference(),
	queue           = queue:new() :: queue:queue({term(), {pid(), reference()}}),
	digests         = []          :: [{file:filename_all(), <<_:128>>}],
	release         = undefined   :: undefined | file:filename_all(),
	request         = undefined   :: undefined | {reference(), <<_:128>>, file:filename_all()}
}).

%% Macros
-define(RETRY_AFTER, 10000). % timer:seconds(10).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Key, DigestURL, CompressedURL) ->
	gen_server:start_link({via, geolite2data_server, Key}, ?MODULE, {Key, DigestURL, CompressedURL}, []).

child_spec(Key, DigestURL, CompressedURL) ->
	{{geolite2data_database, Key},
		{geolite2data_database, start_link, [Key, DigestURL, CompressedURL]},
		transient, 5000, worker, [geolite2data_database]}.

get_database(Key, Timeout) ->
	gen_server:call({via, geolite2data_server, Key}, get_database, Timeout).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init({Key, DigestURL, CompressedURL}) ->
	State = #state{key=Key, digest_url=DigestURL, compressed_url=CompressedURL},
	{ok, start_timer(State, 0)}.

%% @private
handle_call(Request=get_database, From, State=#state{queue=Queue, release=undefined}) ->
	{noreply, State#state{queue=queue:in({Request, From}, Queue)}};
handle_call(get_database, _From, State=#state{release=Release}) ->
	{reply, {ok, Release}, State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

%% @private
handle_cast(config_change, State) ->
	{noreply, start_timer(State, 0)};
handle_cast(_Request, State) ->
	{noreply, State}.

%% @private
handle_info({timeout, TimerReference, poll}, State=#state{name=idle, timer_reference=TimerReference}) ->
	maybe_poll(State#state{timer_reference=undefined}, os:system_time(seconds));
handle_info({http, {RequestId, saved_to_file}}, State=#state{name=wait, key=Key, request={RequestId, Digest, Filename}}) ->
	case geolite2data_hash:file(md5, Filename, maybe_compressed(Filename)) of
		{ok, Digest} ->
			ok = geolite2data_event:announce(Key, Filename),
			{noreply, maybe_cleanup(State#state{release=Filename, request=undefined})};
		_ ->
			_ = file:delete(Filename),
			{noreply, start_timer(State, ?RETRY_AFTER)}
	end;
handle_info({http, {_RequestId, _}}, State=#state{name=wait}) ->
	{noreply, start_timer(State, ?RETRY_AFTER)};
handle_info(_Info, State) ->
	{noreply, State}.

%% @private
terminate(_Reason, _State) ->
	ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @private
cleanup(State=#state{queue=Queue}, Digests, Release) ->
	KeepReleases = case geolite2data:keep_releases() of
		Integer when is_integer(Integer) andalso Integer >= 1 ->
			Integer;
		_ ->
			1
	end,
	Filenames = droplast([F || {F, _} <- Digests, F =/= Release] ++ [Release], KeepReleases),
	_ = [file:delete(Filename) || Filename <- Filenames],
	flush_queue(Queue, State#state{digests=#{}, queue=queue:new()}).

%% @private
droplast([], _) ->
	[];
droplast(List, 0) ->
	List;
droplast(List, N) ->
	droplast(lists:droplast(List), N - 1).

%% @private
flush_queue(Queue0, State0) ->
	case queue:out(Queue0) of
		{{value, {Request, From}}, Queue1} ->
			case handle_call(Request, From, State0) of
				{noreply, State1} ->
					flush_queue(Queue1, State1);
				{reply, Reply, State1} ->
					_ = gen_server:reply(From, Reply),
					flush_queue(Queue1, State1)
			end;
		{empty, Queue0} ->
			State0
	end.

%% @private
get_autoupdate_timeout() ->
	case geolite2data:autoupdate() of
		true ->
			timer:hours(24) div timer:seconds(1);
		Integer when is_integer(Integer) andalso Integer > 0 ->
			Integer;
		_ ->
			false
	end.

%% @private
get_database_directory(#state{key=Key}, DataDir) ->
	ReleasesDir = get_releases_directory(DataDir),
	DatabaseDir = filename:join(ReleasesDir, atom_to_list(Key)),
	ok = filelib:ensure_dir(filename:join(DatabaseDir, ".keep")),
	DatabaseDir.

%% @private
get_next_release_prefix() ->
	{{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:timestamp()),
	lists:flatten(io_lib:format("~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w", [Year, Month, Day, Hour, Minute, Second])).

%% @private
get_releases_directory(DataDir) ->
	ReleasesDir = filename:join(DataDir, "releases"),
	ok = filelib:ensure_dir(filename:join(ReleasesDir, ".keep")),
	ReleasesDir.

%% @private
is_release_outdated(State=#state{key=Key, release=undefined}, Digests) when length(Digests) > 0 ->
	{Filename, _} = lists:last(Digests),
	ok = geolite2data_event:announce(Key, Filename),
	is_release_outdated(State#state{release=Filename}, Digests);
is_release_outdated(State=#state{key=Key, digest_url=DigestURL}, Digests) ->
	case httpc:request(get, {DigestURL, []}, [], [{body_format, binary}]) of
		{ok, {{_, 200, _}, _, HexDigest}} ->
			Digest = geolite2data_hex:hex_to_bin(HexDigest),
			case lists:keyfind(Digest, 2, Digests) of
				{Filename, Digest} ->
					ok = geolite2data_event:announce(Key, Filename),
					{false, State#state{release=Filename}};
				false ->
					{true, Digest, State}
			end;
		_ ->
			{true, State}
	end.

%% @private
list_release_digests(State, DataDir) ->
	DatabaseDir = get_database_directory(State, DataDir),
	Folder = fun(Filename, Acc) ->
		try geolite2data_hash:file(md5, Filename, maybe_compressed(Filename)) of
			{ok, Digest} ->
				ordsets:add_element({Filename, Digest}, Acc);
			_ ->
				Acc
		catch
			_:_ ->
				Acc
		end
	end,
	Digests = filelib:fold_files(DatabaseDir, [], false, Folder, ordsets:new()),
	{State#state{digests=Digests}, Digests}.

%% @private
maybe_cleanup(State=#state{digests=Digests, release=Release, request=undefined}) when Release =/= undefined ->
	case get_autoupdate_timeout() of
		false ->
			start_timer(cleanup(State, Digests, Release), timer:hours(1));
		Autoupdate ->
			start_timer(cleanup(State, Digests, Release), timer:seconds(Autoupdate))
	end;
maybe_cleanup(State) ->
	State.

%% @private
maybe_compressed(Filename) ->
	case filename:extension(Filename) of
		".gz" ->
			[compressed];
		_ ->
			[]
	end.

%% @private
maybe_poll(State=#state{last_checked=LastChecked}, Now) ->
	case get_autoupdate_timeout() of
		false ->
			{noreply, start_timer(State, timer:hours(1))};
		Autoupdate ->
			case (Now - LastChecked) >= Autoupdate of
				false ->
					{noreply, start_timer(State, timer:seconds((LastChecked + Autoupdate) - Now))};
				true ->
					poll(State#state{name=poll})
			end
	end.

%% @private
poll(State0=#state{key=Key}) ->
	case [K || {K, _, _} <- geolite2data:list_databases(), K =:= Key] of
		[Key] ->
			ok = geolite2data_event:refresh(Key),
			DataDir = geolite2data:data_dir(),
			{State1, Digests} = list_release_digests(State0, DataDir),
			case is_release_outdated(State1, Digests) of
				{true, State2} ->
					{noreply, start_timer(State2, ?RETRY_AFTER)};
				{true, Digest, State2} ->
					request_release(State2, DataDir, Digest);
				{false, State2} ->
					{noreply, maybe_cleanup(State2)}
			end;
		_ ->
			{stop, normal, State0}
	end.

%% @private
request_release(State=#state{compressed_url=CompressedURL}, DataDir, Digest) ->
	DatabaseDir = get_database_directory(State, DataDir),
	Filename = filename:join(DatabaseDir, get_next_release_prefix() ++ filename:basename(CompressedURL)),
	_ = file:delete(Filename),
	case httpc:request(get, {CompressedURL, []}, [], [{receiver, self()}, {stream, Filename}, {sync, false}]) of
		{ok, RequestId} ->
			{noreply, State#state{name=wait, request={RequestId, Digest, Filename}}};
		_ ->
			{noreply, start_timer(State, ?RETRY_AFTER)}
	end.

%% @private
start_timer(State=#state{timer_reference=undefined, request=undefined}, Time) ->
	TimerReference = erlang:start_timer(Time, self(), poll),
	State#state{name=idle, timer_reference=TimerReference};
start_timer(State=#state{timer_reference=undefined, request={RequestId, _, Filename}}, Time) ->
	ok = httpc:cancel_request(RequestId),
	_ = file:delete(Filename),
	start_timer(State#state{request=undefined}, Time);
start_timer(State=#state{timer_reference=TimerReference}, Time) ->
	catch erlang:cancel_timer(TimerReference),
	start_timer(State#state{timer_reference=undefined}, Time).
