-module(displaytest_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link(SpiSettings) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SpiSettings]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([SpiSettings]) ->
    SupFlags = {one_for_one, 1, 1},
    ChildSpecs = [
        {epd, {epd, start_link, [SpiSettings]}, permanent, brutal_kill, worker, [epd]}
    ],
    {ok, {SupFlags, ChildSpecs}}.
