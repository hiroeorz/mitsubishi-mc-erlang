%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2018, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2018 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mitsubishi_mc).

%% API
-export([start_port/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc start port server.
%% @end
%%--------------------------------------------------------------------
-spec start_port(SrcIP, Port) -> supervisor:startchild_ret() when
      SrcIP :: inet:ip_address(),
      Port :: inet:port_number().
start_port(SrcIP, Port) ->
    _ = application:start(mitsubishi_mc),
    Child = {{mitsubishi_mc_port, Port}, 
	     {mitsubishi_mc_port, start_link, [SrcIP, Port]},
	     permanent, 2000, worker, [mitsubishi_mc_port]},
    supervisor:start_child(mitsubishi_mc_sup, Child).

%%%===================================================================
%%% Internal functions
%%%===================================================================
