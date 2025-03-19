%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2018, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2018 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mitsubishi_mc_driver).

%% Include
-include("mitsubishi_mc.hrl").

%% API
-export([command/6,
	 get_process_identifier/1,
	 parse_response/3]).

%% for debug
-export([fmt/8,
	 sub_header/2,
	 access_route/4,
	 watch_timer/1,
	 request_data_length/2,
	 device/3
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc 送信コマンド伝文を生成する。
%%
%% Timeoutはミリセカンドで、自局の場合は250ミリ秒 - 10000ミリ秒が望ましい.
%% @end
%%--------------------------------------------------------------------
-spec command(FrameType, SerialNo, TimeoutMSec, NetworkNo, PcNo, list()) -> binary() when
      FrameType :: frame_type(),
      SerialNo :: non_neg_integer(),
      TimeoutMSec :: non_neg_integer(),
      NetworkNo :: non_neg_integer(),
      PcNo :: non_neg_integer().
command(FrameType, SerialNo, TimeoutMSec, NetworkNo, PcNo, [?CODE_READ_IO, ?SUB_CODE_READ_IO, No, Code, Count]) ->
    RequestBin = <<?CODE_READ_IO:16/little-unsigned-integer,
		   ?SUB_CODE_READ_IO:16/little-unsigned-integer,
		   (device(No, Code, Count))/binary >>,
    fmt(FrameType, SerialNo, NetworkNo, PcNo, 16#03FF, 0, TimeoutMSec, RequestBin);

command(FrameType, SerialNo, TimeoutMSec, NetworkNo, PcNo,
	[?CODE_WRITE_IO, ?SUB_CODE_WRITE_IO, No, Code, Count, ValList]) ->
    RequestBin = <<?CODE_WRITE_IO:16/little-unsigned-integer,
		   ?SUB_CODE_WRITE_IO:16/little-unsigned-integer,
		   (device(No, Code, Count))/binary,
		   (vallist_to_binary(2, ValList))/binary >>,

    TimeoutVal = TimeoutMSec div 250,
    fmt(FrameType, SerialNo, NetworkNo, PcNo, 16#03FF, 0, TimeoutVal, RequestBin).

%%--------------------------------------------------------------------
%% @doc 受信伝文を受け取ってからシリアル番号を返す(4E専用)。
%%
%% 未テスト
%% @end
%%--------------------------------------------------------------------
-spec get_process_identifier(binary()) -> non_neg_integer().
get_process_identifier(Bin) ->
    <<16#D4:8/unsigned-integer,
      16#00:8/unsigned-integer,
      SerialNo:16/little-unsigned-integer,
      0:16/unsigned-integer,
      _:40/little-unsigned-integer,
      Size:16/little-unsigned-integer,
      _BodyBin:(Size)/binary>> = Bin,

    SerialNo.

%%--------------------------------------------------------------------
%% @doc 受信伝文バイナリをパースして返す.
%%
%% 未テスト
%% @end
%%--------------------------------------------------------------------
-spec parse_response(Command, SubCommand, binary()) -> ok | {ok, term()} | {error, non_neg_integer()} when
      Command :: non_neg_integer(),
      SubCommand :: non_neg_integer().

%% 3E
parse_response(Command, SubCommand, <<16#D0:8/unsigned-integer, _/binary>> = Bin) ->
    <<16#D0:8/unsigned-integer,
      16#00:8/unsigned-integer,
      _:40/little-unsigned-integer,
      Size:16/little-unsigned-integer,
      BodyBin:(Size)/binary>> = Bin,
    parse_response_body(Command, SubCommand, BodyBin);

%% 4E
parse_response(Command, SubCommand, <<16#D4:8/unsigned-integer, _/binary>> = Bin) ->
    <<16#D4:8/unsigned-integer,
      16#00:8/unsigned-integer,
      _SerialNo:16/little-unsigned-integer,
      0:16/unsigned-integer,
      _:40/little-unsigned-integer,
      Size:16/little-unsigned-integer,
      BodyBin:(Size)/binary>> = Bin,
    parse_response_body(Command, SubCommand, BodyBin).

%%--------------------------------------------------------------------
%% @private
%% @doc 受信伝文のデータ部分をパースして返す。エラーの場合はエラーコードを返す.
%% @end
%%--------------------------------------------------------------------
-spec parse_response_body(CommandCode, SubCode, binary()) -> ok | {ok, term()} | {error, term()} when
      CommandCode :: non_neg_integer(),
      SubCode :: non_neg_integer().
%% OK
parse_response_body(_CommandCode, _SubCode, <<00:16/little-unsigned-integer>>) ->
    ok;

%% OK and Data returned
parse_response_body(CommandCode, SubCode, <<00:16/little-unsigned-integer, DataBin/binary>>) ->
    parse_body_detail(CommandCode, SubCode, DataBin);

%% NG
parse_response_body(_CommandCode, _SubCode, <<FinishCode:16/little-unsigned-integer, _/binary>>)
  when FinishCode =/= 0 ->
    {error, FinishCode}.

%%--------------------------------------------------------------------
%% @private
%% @doc 受信伝文をパースして意味のある値にして返す.
%% @end
%%--------------------------------------------------------------------
-spec parse_body_detail(CommandCode, SubCode, Bin) -> {ok, term()} when
      CommandCode :: non_neg_integer(),
      SubCode :: non_neg_integer(),
      Bin :: binary().
parse_body_detail(?CODE_READ_IO, ?SUB_CODE_READ_IO, Bin) ->
    ValList = binary_to_vallist(2, Bin),
    {ok, ValList};

parse_body_detail(?CODE_WRITE_IO, ?SUB_CODE_WRITE_IO, Bin) ->
    ValList = binary_to_vallist(2, Bin),
    {ok, ValList}.

%%--------------------------------------------------------------------
%% @private
%% @doc バイナリデータを受け取って、数値のリストを生成して返す.
%% @end
%%--------------------------------------------------------------------
-spec binary_to_vallist(pos_integer(), binary()) -> [non_neg_integer()].
binary_to_vallist(UnitByte, Bin) ->
    binary_to_vallist(UnitByte, Bin, []).

binary_to_vallist(_UnitByte, <<>>, Result) ->
    lists:reverse(Result);

binary_to_vallist(2, <<V:16/little-unsigned-integer, Tail/binary>>, Result) ->
    binary_to_vallist(2, Tail, [V | Result]).

%%--------------------------------------------------------------------
%% @private
%% @doc 数値のリストを受け取って、バイナリデータを生成して返す.
%% @end
%%--------------------------------------------------------------------
-spec vallist_to_binary(pos_integer(), [non_neg_integer()]) -> binary().
vallist_to_binary(UnitByte, List) ->
    vallist_to_binary(UnitByte, List, []).

vallist_to_binary(_UnitByte, [], Result) ->
    list_to_binary(lists:reverse(Result));

vallist_to_binary(2, [Val | Tail], Result) ->
    vallist_to_binary(2, Tail, [<<Val:16/little-unsigned-integer>> | Result]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 全てのデータを受け取って送信するバイナリ伝文を生成する。
%% @end
%%--------------------------------------------------------------------
-spec fmt(FrameType, SerialNo, NetworkNo, PcNo, UnitIONo, UnitNo, WatchTimerMSec, RequestBin) -> binary() when
      FrameType :: frame_type(),
      SerialNo :: non_neg_integer(),
      NetworkNo :: non_neg_integer(),
      PcNo :: non_neg_integer(),
      UnitIONo :: non_neg_integer(),
      UnitNo :: non_neg_integer(),
      WatchTimerMSec :: non_neg_integer(),
      RequestBin :: binary().
fmt(FrameType, SerialNo, NetworkNo, PcNo, UnitIONo, UnitNo, WatchTimerMSec, RequestBin) ->
    WatchTimerBin = watch_timer(WatchTimerMSec),
    Len = request_data_length(WatchTimerBin, RequestBin),

    <<(sub_header(FrameType, SerialNo))/binary,
      (access_route(NetworkNo, PcNo, UnitIONo, UnitNo))/binary,
      Len/binary,
      WatchTimerBin/binary,
      RequestBin/binary >>.

%%--------------------------------------------------------------------
%% @private
%% @doc サブヘッダ
%% @end
%%--------------------------------------------------------------------
-spec sub_header(FrameType, SerialNo) -> binary() when
      FrameType :: frame_type(),
      SerialNo :: non_neg_integer().
sub_header('3E', SerialNo)
  when is_integer(SerialNo), 
       Serialno >= 16#0000,
       SerialNo =< 16#FFFF ->
    <<16#50:8, 00:8>>;

sub_header('4E', SerialNo)
  when is_integer(SerialNo), 
       Serialno >= 16#0000,
       SerialNo =< 16#FFFF ->
    <<16#54:8, 00:8,  SerialNo:16/little-unsigned-integer, 0:16/unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc アクセス経路
%%
%% PCに直接接続された自局の場合は、
%%   NetworkNo = 0
%%   PcNo = 16#ff
%%   UnitIONo = 0,
%%   UnitNo = 0
%% を設定します。
%% @end
%%--------------------------------------------------------------------
-spec access_route(NetworkNo, PcNo, UnitIONo, UnitNo) -> binary() when
      NetworkNo :: non_neg_integer(),
      PcNo :: non_neg_integer(),
      UnitIONo :: non_neg_integer(),
      UnitNo :: non_neg_integer().
access_route(NetworkNo, PcNo, UnitIONo, UnitNo)
  when is_integer(NetworkNo),
       is_integer(PcNo),
       is_integer(UnitIONo),
       is_integer(UnitNo) ->
    <<NetworkNo:8/unsigned-integer,
      PcNo:8/unsigned-integer,
      UnitIONo:16/little-unsigned-integer,
      UnitNo:8/unsigned-integer >>.

%%--------------------------------------------------------------------
%% @private
%% @doc 監視タイマ. 0は無限待ち
%% @end
%%--------------------------------------------------------------------
-spec watch_timer(MSec) -> binary() when
      MSec :: non_neg_integer().
watch_timer(MSec)
  when is_integer(MSec) ->
    <<MSec:16/little-unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc 要求データ長
%% @end
%%--------------------------------------------------------------------
-spec request_data_length(WatchTimerBin, RequestBin) -> binary() when
      WatchTimerBin :: binary(),
      RequestBin :: binary().
request_data_length(WatchTimerBin, RequestBin)
  when is_binary(WatchTimerBin),
       is_binary(RequestBin) ->
    Bytes = byte_size(WatchTimerBin) + byte_size(RequestBin),
    <<Bytes:16/little-unsigned-integer>>.

%%--------------------------------------------------------------------
%% @private
%% @doc コマンド内でのデバイス指定
%%
%% データレジスタのデバイスコードは16#A8.
%% @end
%%--------------------------------------------------------------------
-spec device(No, Code, Count) -> binary() when
      No :: non_neg_integer(),
      Code :: non_neg_integer(),
      Count :: non_neg_integer().
device(No, Code, Count)
  when is_integer(No),
       is_integer(Code) ->
    <<No:24/little-unsigned-integer,
      Code:8/unsigned-integer,
      Count:16/little-unsigned-integer >>.
