%%%-------------------------------------------------------------------
%%% @author HIROE Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created :  8 Aug 2013 by HIROE Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(mitsubishi_mc_port).

-behaviour(gen_server).

%% Include
-include("mitsubishi_mc.hrl").

%% Include if TEST
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/2,
	 start_link/3, 
	 send_command/3]).

%% DEBUG
-export([unpack_hex/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


-record(state, {port                     :: inet:port_number(),
		ip_address               :: inet:ip_address(),
		socket                   :: gen_udp:socket(),
		process_tbl = dict:new() :: dict:dict(),
		frame_type = '3E'        :: frame_type(),
		identifier = 1           :: non_neg_integer()}).

-define(RESPONSE_TIMEOUT, 3000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(SrcIPAddress, Port) -> {ok, pid()} | 
					ignore | 
					{error, atom()} when
      SrcIPAddress :: inet:ip_address(),
      Port :: inet:port_number().
start_link(SrcIPAddress, Port) ->
    start_link(SrcIPAddress, Port, '3E').

-spec start_link(SrcIPAddress, Port, FrameType) -> {ok, pid()} | 
						   ignore | 
						   {error, atom()} when
      SrcIPAddress :: inet:ip_address(),
      Port :: inet:port_number(),
      FrameType :: frame_type().
start_link(SrcIPAddress, Port, FrameType) ->
    gen_server:start_link(?MODULE, [Port, SrcIPAddress, FrameType], []).

%%--------------------------------------------------------------------
%% @doc Send fins command to PLC.
%% @end
%%--------------------------------------------------------------------
-spec send_command(DstIP, Port, Command) -> ok | 
					    {ok, Data} |
					    {error, inet:posix()} |
					    inet:posix() when
      DstIP :: inet:ip_address(),
      Port :: inet:port_number(),
      Command :: list(),
      Data :: term().
send_command(DstIP, Port, [CommandCode, SubCode | _] = Command) ->
    case mitsubishi_mc_port_manager:get_pid(Port) of
	{ok, Pid} ->
	    case gen_server:call(Pid, {send_command, DstIP, Command}) of
		{ok, async} ->
		    wait_response(CommandCode, SubCode);
		ok ->
		    ok;
		{ok, Result} ->
		    {ok, Result};
		{error, timeout} ->
		    error_logger:error_msg("plc network is down.");
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, not_found} ->
	    {error, port_not_started}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
init([Port, SrcIPAddress, FrameType]) when is_binary(SrcIPAddress);
					   is_list(SrcIPAddress) ->
    init([Port, to_tuple_address(SrcIPAddress), FrameType]);

init([Port, SrcIPAddress, FrameType]) when is_tuple(SrcIPAddress) ->
    Active = socket_active(FrameType),
    {ok, Sock} = gen_udp:open(Port, [{ip, SrcIPAddress}, binary, {active, Active}]),
    ok = mitsubishi_mc_port_manager:set_pid(Port, self()),
    {ok, #state{port = Port,
		ip_address = SrcIPAddress,
		socket = Sock,
		frame_type = FrameType}}.

%%--------------------------------------------------------------------
%% @private
%% @doc フレーム種別によってUDPソケットのアクティブタイプを返す.
%% @end
%%--------------------------------------------------------------------
-spec socket_active(frame_type()) -> boolean().
socket_active('3E') -> false;
socket_active('4E') -> true.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send_command, DstIP, Command}, From, State) 
  when is_binary(DstIP);
       is_list(DstIP) ->
    handle_call({send_command, to_tuple_address(DstIP), Command}, From, State);

handle_call({send_command, DstIP, Command}, {Pid, _Ref},
	    State = #state{identifier = Identifier, port = Port, socket = Sock, frame_type = FrameType}) ->
    [CommandCode, SubCode | _] = Command,
    NewState = set_process_identifier(FrameType, Pid, State),
    Bin = mitsubishi_mc_driver:command(FrameType, Identifier, 1000, 0, 16#ff, Command),

    case gen_udp:send(Sock, DstIP, Port, Bin) of
	ok ->
	    case FrameType of
		'4E' ->
		    {reply, {ok, async}, NewState};
                '3E' ->
		    Result = recv_packet(Sock, CommandCode, SubCode),
                    {reply, Result, NewState}
            end;
	{error, Reason} ->
	    {reply, {error, Reason}, NewState}
    end;

%% for test
handle_call(reset_identifier, _From, State) ->
    {reply, ok, State#state{identifier = 1}};

%% for test
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% プロセス通信で受信したUDPソケットのデータを非同期で呼び出し元に返す(4E専用).
handle_info({udp, _Sock, _Host, _Port, Bin}, State) ->
    %%io:format("recv info: ~p~n", [Bin]),
    Identifier = mitsubishi_mc_driver:get_process_identifier(Bin),

    NewState = case get_process_pid(Identifier, State) of
		   {ok, Pid} ->
		       Pid ! {ok, Bin},
		       delete_process_identifier(Identifier, State);
		   error ->
		       State
	       end,
    {noreply, NewState};

handle_info(Info, State) ->
    io:format("unknown info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    Sock = State#state.socket,
    ok = gen_udp:close(Sock).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc プロセスと識別数値の組み合わせを保存したStateをかえす(実際に識別値を保存するのは4Eのみ)
%% @end
%%--------------------------------------------------------------------
-spec set_process_identifier(frame_type(), pid(), #state{}) -> #state{}.
set_process_identifier('3E', _Pid, State) ->
    State;

set_process_identifier('4E', Pid, State) ->
    Dict = State#state.process_tbl,
    Identifier = State#state.identifier,
    NewDict = dict:store(Identifier, Pid, Dict),
    
    NextIdentifier = if Identifier >= 16#FFFF ->
			    1;
		       true ->
			    Identifier + 1
		    end,

    State#state{process_tbl = NewDict, identifier = NextIdentifier}.

%%--------------------------------------------------------------------
%% @private
%% @doc プロセスと識別数値の組み合わせを削除したStateをかえす
%% @end
%%--------------------------------------------------------------------
-spec delete_process_identifier(Identifier, #state{}) -> #state{} when
      Identifier :: non_neg_integer().
delete_process_identifier(Identifier, State) ->
    Dict = State#state.process_tbl,
    NewDict = dict:erase(Identifier, Dict),
    State#state{process_tbl = NewDict}.

%%--------------------------------------------------------------------
%% @private
%% @doc 渡された識別数値にひもづけられたプロセス識別子をかえす
%% @end
%%--------------------------------------------------------------------
-spec get_process_pid(Identifier, #state{}) -> pid() | error when
      Identifier :: non_neg_integer().
get_process_pid(Identifier, State) ->
    Dict = State#state.process_tbl,
    dict:find(Identifier, Dict).

%%--------------------------------------------------------------------
%% @private
%% @doc gen_serverからの非同期応答を待機し、受け取った値をかえす(4E専用)
%% @end
%%--------------------------------------------------------------------
-spec wait_response(non_neg_integer(), non_neg_integer()) -> ok |
							     {ok, term()} |
							     {error, timeout} | 
							     {error, non_neg_integer()}.
wait_response(CommandCode, SubCode) ->
    receive
	{ok, Bin} ->
	    case mitsubishi_mc_driver:parse_response(CommandCode, SubCode, Bin) of
		{error, ErrCode} -> {error, ErrCode};
		ok                 -> ok;
		{ok, Val}          -> {ok, Val}
	    end;
	{error, Reason} ->
	    {error, Reason}
    after ?RESPONSE_TIMEOUT ->
	    {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc UDPソケットからデータを読み込んで返す(3E専用)。
%% @end
%%--------------------------------------------------------------------
-spec recv_packet(Sock, CommandCode, SubCode) -> {ok, binary()} | {error, any()} when
      Sock :: gen_udp:socket(),
      CommandCode :: non_neg_integer(),
      SubCode :: non_neg_integer().
recv_packet(Sock, CommandCode, SubCode) ->
    case gen_udp:recv(Sock, 0) of
	{ok, {_Address, _Port, PacketBin}} ->
	    mitsubishi_mc_driver:parse_response(CommandCode, SubCode, PacketBin);
	{error, Reason1} ->
	    {error, Reason1}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Unpack hex string for log output.
%% @end
%%--------------------------------------------------------------------
-spec unpack_hex(binary()) -> binary().
unpack_hex(Bin) ->
    List = binary_to_list(Bin),
    StrList = [string:right(integer_to_list(Unit, 16), 2, $0) || Unit <- List],
    list_to_binary(StrList).

%%--------------------------------------------------------------------
%% @private
%% @doc Transfer string ipaddress(eg."192.168.0.1") to tuple(eg.{192,168,0,1}).
%% @end
%%--------------------------------------------------------------------
-spec to_tuple_address(binary() | list()) -> tuple().
to_tuple_address(SrcIPAddress) when is_binary(SrcIPAddress) ->
    to_tuple_address(binary_to_list(SrcIPAddress));

to_tuple_address(SrcIPAddress) when is_list(SrcIPAddress) ->
    StrList = re:split(SrcIPAddress,"[\.]",[{return, list}]),
    [A1, A2, A3, A4] = [list_to_integer(A) || A <- StrList],
    {A1, A2, A3, A4}.
