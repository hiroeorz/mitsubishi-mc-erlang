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
      Command :: tuple(),
      Data :: term().
send_command(DstIP, Port, Command) ->
    case mitsubishi_mc_port_manager:get_pid(Port) of
	{ok, Pid} ->
	    case gen_server:call(Pid, {send_command, DstIP, Command}) of
		{ok, async} ->
		    wait_response();
		{ok, Result} ->
		    {ok, Result};
		{error, Reason} ->
		    error_logger:error_msg("plc network is down."),
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
    Active = case FrameType of
		 '3E' -> false;
		 '4E' -> true
	     end,

    {ok, Sock} = gen_udp:open(Port, [{ip, SrcIPAddress}, binary, {active, Active}]),
    ok = mitsubishi_mc_port_manager:set_pid(Port, self()),
    {ok, #state{port = Port,
		ip_address = SrcIPAddress,
		socket = Sock,
		frame_type = FrameType}}.

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

handle_call({send_command, DstIP, Command}, {Pid, _Ref}, State) ->
    Identifier = State#state.identifier,
    Port = State#state.port,
    Sock = State#state.socket,
    FrameType = State#state.frame_type,
    NewState = set_process_identifier(Pid, State),
    Bin = mitsubishi_mc_driver:command(Identifier, 1000, 0, 16#ff, Command),
    io:format("send command: ~p~n", [Bin]),

    case gen_udp:send(Sock, DstIP, Port, Bin) of
	ok ->
	    case FrameType of
		'4E' ->
		    {reply, {ok, async}, NewState};
                '3E' ->
		    Result = recv_packet(Sock),
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
handle_info({udp, _Sock, _Host, _Port, Bin}, State) ->
    io:format("recv info: ~p~n", [Bin]),
    %%Identifier = mitsubishi_mc_driver:get_process_identifier(Bin),

    %%NewState = case get_process_pid(Identifier, State) of
%%		   {ok, Pid} ->
%%		       Pid ! {ok, Bin};
%%		       %%delete_process_identifier(Identifier, State);
%%		   error ->
%%		       State
%%	       end,
%%    {noreply, NewState};
    {noreply, State};

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
%% @doc プロセスと識別数値の組み合わせを保存したStateをかえす
%% @end
%%--------------------------------------------------------------------
-spec set_process_identifier(pid(), #state{}) -> #state{}.
set_process_identifier(Pid, State) ->
    Dict = State#state.process_tbl,
    Identifier = State#state.identifier,
    NewDict = dict:store(Identifier, Pid, Dict),
    
    NextIdentifier = if Identifier >= 16#FF ->
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
%%-spec delete_process_identifier(Identifier, #state{}) -> #state{} when
%%      Identifier :: non_neg_integer().
%%delete_process_identifier(Identifier, State) ->
%%    Dict = State#state.process_tbl,
%%    NewDict = dict:erase(Identifier, Dict),
%%    State#state{process_tbl = NewDict}.

%%--------------------------------------------------------------------
%% @private
%% @doc 渡された識別数値にひもづけられたプロセス識別子をかえす
%% @end
%%--------------------------------------------------------------------
%%-spec get_process_pid(Identifier, #state{}) -> pid() | error when
%%      Identifier :: non_neg_integer().
%%get_process_pid(Identifier, State) ->
%%    Dict = State#state.process_tbl,
%%    dict:find(Identifier, Dict).

%%--------------------------------------------------------------------
%% @private
%% @doc gen_serverからの非同期応答を待機し、受け取った値をかえす
%% @end
%%--------------------------------------------------------------------
%%-spec wait_response() -> term() | 
%%			 {error, timeout} | 
%%			 {error, {non_neg_integer(), non_neg_integer()}}.
wait_response() ->
    receive
	{ok, Bin} ->
	    case mitsubishi_mc_driver:parse_response(Bin) of
		{error, {ErrCode}} -> {error, ErrCode};
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

%%--------------------------------------------------------------------
%% @private
%% @doc UDPソケットからデータを読み込んで返す(フレームが3Eの場合に呼ばれる)。
%% @end
%%--------------------------------------------------------------------
-spec recv_packet(Sock) -> {ok, binary()} | {error, any()} when
      Sock :: gen_udp:socket().
recv_packet(Sock) ->
    case gen_udp:recv(Sock, 0) of
	{ok, {_Address, _Port, PacketBin}} ->
	    <<16#D0:8/unsigned-integer,
	      16#00:8/unsigned-integer,
	      _:40/little-unsigned-integer,
	      Size:16/little-unsigned-integer,
	      _BodyBin:(Size)/binary>> = PacketBin, %% check

	    {ok, PacketBin};
	{error, Reason1} ->
	    {error, Reason1}
    end.
