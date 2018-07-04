%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2018, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2018 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mitsubishi_mc).

%% Include
-include("mitsubishi_mc.hrl").

%% API
-export([start_port/2,
	 read_dm_values/4,
	 write_dm_values/4,
	 write_dm_same_value/5]).

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
    start_port(SrcIP, Port, '3E').

-spec start_port(SrcIP, Port, FrameType) -> supervisor:startchild_ret() when
      SrcIP :: inet:ip_address(),
      Port :: inet:port_number(),
      FrameType :: frame_type().
start_port(SrcIP, Port, FrameType) ->
    _ = application:start(mitsubishi_mc),
    Child = {{mitsubishi_mc_port, Port}, 
	     {mitsubishi_mc_port, start_link, [SrcIP, Port, FrameType]},
	     permanent, 2000, worker, [mitsubishi_mc_port]},
    supervisor:start_child(mitsubishi_mc_sup, Child).

%%--------------------------------------------------------------------
%% @doc read values from Register area.
%% @end
%%--------------------------------------------------------------------
-spec read_dm_values(DstIP, Port, StartAddress, Count) -> {ok, [non_neg_integer()]} |
							  {error, non_neg_integer()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartAddress :: non_neg_integer(),
      Count :: non_neg_integer().
read_dm_values(DstIP, Port, StartAddress, Count) ->
    Command = [?CODE_READ_IO, ?SUB_CODE_READ_IO, StartAddress, 16#A8, Count],
    mitsubishi_mc_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc write values to Register area.
%% @end
%%--------------------------------------------------------------------
-spec write_dm_values(DstIP, Port, StartAddress, List) -> ok | {error, non_neg_integer()} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartAddress :: non_neg_integer(),
      List :: [non_neg_integer()].
write_dm_values(DstIP, Port, StartAddress, List) ->
    Command = [?CODE_WRITE_IO, ?SUB_CODE_WRITE_IO, StartAddress, 16#A8, length(List), List],
    mitsubishi_mc_port:send_command(DstIP, Port, Command).

%%--------------------------------------------------------------------
%% @doc write same value to Register area.
%% @end
%%--------------------------------------------------------------------
-spec write_dm_same_value(DstIP, Port, StartAddress, Count, Value) -> ok | {error, non_neg_integer} when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      StartAddress :: non_neg_integer(),
      Count :: non_neg_integer(),
      Value :: non_neg_integer().
write_dm_same_value(DstIP, Port, StartAddress, Count, Value) ->
    List = [Value || _ <- lists:seq(1, Count)],
    write_dm_values(DstIP, Port, StartAddress, List).

%%%===================================================================
%%% Internal functions
%%%===================================================================
