%%%-------------------------------------------------------------------
%% @doc json_proto top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(json_proto_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    Flags = #{strategy => one_for_all},
%    ProtoChild = #{id     => json_proto,
%                 start  => {json_proto, start_link, []},
%                 restart    => permanent,
%                 shutdown   => 2000,
%                 type   => worker,
%                 modules    => [json_proto]},
    {ok, {Flags, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
