# geolite2data

[![Hex.pm](https://img.shields.io/hexpm/v/geolite2data.svg)](https://hex.pm/packages/geolite2data)

Downloads and updates the [GeoLite2 databases from MaxMind](https://dev.maxmind.com/geoip/geoip2/geolite2/) for Erlang and Elixir.

Designed to be used alongside other applications like [geolix](https://github.com/mneudert/geolix).

## Installation

Add `geolite2data` to your project's dependencies in `mix.exs`

```elixir
defp deps do
  [
    {:geolite2data, "~> 1.0.0"}
  ]
end
```

Add `geolite2data` to your project's dependencies in your `Makefile` for [`erlang.mk`](https://github.com/ninenines/erlang.mk) or the following to your `rebar.config`

```erlang
{deps, [
  {geolite2data, ".*", {git, "git://github.com/potatosalad/erlang-geolite2data.git", {branch, "master"}}}
]}.
```

## Configuration

When using `mix` along with [geolix](https://github.com/mneudert/geolix), the following configuration may be used:

```elixir
config :geolite2data,
  geolix_updater: true
```

This will automatically check for updates once a day and load any changes into geolix.

The defaults for all configuration options are:

```elixir
config :geolite2data,
  autoupdate: 86400, # 24 hours
  databases: [
    {:city, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.md5", "https://geolite.maxmind.com/download/geoip/database/GeoLite2-City.mmdb.gz"},
    {:country, "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.md5", "https://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.mmdb.gz"}
  ],
  geolix_updater: false,
  keep_releases: 2,
  logger: false
```

For Erlang applications, there is an event handler that can be used to watch for any changes to database files:

```erlang
-module(example_geolite2data_listener).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = geolite2data_event:add_handler(geolite2data_event_handler, self()),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'$geolite2data-event', {database, announce, Key, Filename}}, State) ->
    %% Handle changes to the database identified by 'Key' and stored at 'Filename'
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

See `geolite2data_geolix_updater` and `geolite2data_logger` for more examples.

## Usage

The current filename for a database can be fetched with `geolite2data:get_database/1`:

```erlang
geolite2data:get_database(city).
% {ok, ".../priv/releases/country/20160513224618GeoLite2-Country.mmdb.gz"}
```

## License

[MIT](https://opensource.org/licenses/MIT/)
