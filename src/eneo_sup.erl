%%%-------------------------------------------------------------------
%% @doc eneo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eneo_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 100000000,
                 period => 20},
    ChildSpecs = children(),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

children() ->
		[#{id => db,
		   start => {db, start_link, [dbconfig()]},
		   restart => permanent,
		   shutdown => brutal_kill,
		   type => worker,
		   modules => [db]
		  }].

dbconfig() ->
	Host = case os:getenv("PG_HOST") of
		false -> "localhost";
		[] -> "localhost";
		PGHOST -> PGHOST end,
	Username = case os:getenv("PG_USER") of
		false -> "eneo";
		[] -> "eneo";
		PGUSER -> PGUSER 
		end,
	Password = case os:getenv("PG_PASSWORD") of
		false -> "";
		[] -> "";
		PGPW -> PGPW
		end,
	Database = case os:getenv("PG_DATABASE") of
		false -> "eneo";
		[] -> "eneo";
		PGDB -> PGDB
		end,
	 #{host => Host,
		username => Username,
		password => Password,
		database => Database,
		timeout => 4000}.
