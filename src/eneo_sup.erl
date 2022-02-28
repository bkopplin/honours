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

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = children(),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

children() ->
		DbConfig = #{host => "localhost",
						 username => "bjarne",
						 password => "password",
						 database => "bjarne",
						 timeout => 4000
						},
		[#{id => db,
		   start => {db, start_link, [DbConfig]},
		   restart => permanent,
		   shutdown => brutal_kill,
		   type => worker,
		   modules => [db]
		  }].
