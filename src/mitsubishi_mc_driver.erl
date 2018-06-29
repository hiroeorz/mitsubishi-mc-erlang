%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2018, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 29 Jun 2018 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(mitsubishi_mc_driver).

%% API
-export([command/3]).

%% for debug
-export([fmt/3,
	 fmt/7,
	 sub_header/1,
	 access_route/4,
	 watch_timer/1,
	 request_data_length/2,
	 device/3
	]).

-define(CODE_READ_IO,             16#0401).
-define(SUB_CODE_READ_IO_REQUEST, 16#0000).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc 送信コマンド伝文を生成する。
%%
%% Timeoutはミリセカンドで、自局の場合は250ミリ秒 - 10000ミリ秒が望ましい.
%% @end
%%--------------------------------------------------------------------
-spec command(SerialNo, Timeout, tuple()) -> binary() when
      SerialNo :: non_neg_integer(),
      Timeout :: non_neg_integer().
command(SerialNo, Timeout, {?CODE_READ_IO, ?SUB_CODE_READ_IO_REQUEST, No, Code, Count}) ->
    RequestBin = <<?CODE_READ_IO:16/little-unsigned-integer,
		   ?SUB_CODE_READ_IO_REQUEST:16/little-unsigned-integer,
		   (device(No, Code, Count))/binary >>,
    fmt(SerialNo, Timeout, RequestBin).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 全てのデータを受け取って送信するバイナリ伝文を生成する。
%% @end
%%--------------------------------------------------------------------
-spec fmt(SerialNo, WatchTimerMSec, RequestBin) -> binary() when
      SerialNo :: non_neg_integer(),
      WatchTimerMSec :: non_neg_integer(),
      RequestBin :: binary().
fmt(SerialNo, WatchTimerMSec, RequestBin) ->
    fmt(SerialNo, 0, 16#ff, 0, 0, WatchTimerMSec, RequestBin).

-spec fmt(SerialNo, NetworkNo, PcNo, UnitIONo, UnitNo, WatchTimerMSec, RequestBin) -> binary() when
      SerialNo :: non_neg_integer(),
      NetworkNo :: non_neg_integer(),
      PcNo :: non_neg_integer(),
      UnitIONo :: non_neg_integer(),
      UnitNo :: non_neg_integer(),
      WatchTimerMSec :: non_neg_integer(),
      RequestBin :: binary().
fmt(SerialNo, NetworkNo, PcNo, UnitIONo, UnitNo, WatchTimerMSec, RequestBin) ->
    WatchTimerBin = watch_timer(WatchTimerMSec),
    Len = request_data_length(WatchTimerBin, RequestBin),

    <<(sub_header(SerialNo))/binary,
      (access_route(NetworkNo, PcNo, UnitIONo, UnitNo))/binary,
      Len/binary,
      WatchTimerBin/binary,
      RequestBin/binary >>.

%%--------------------------------------------------------------------
%% @private
%% @doc サブヘッダ
%% @end
%%--------------------------------------------------------------------
-spec sub_header(SerialNo) -> binary() when
      SerialNo :: non_neg_integer().

sub_header(SerialNo)
  when is_integer(SerialNo), 
       SerialNo > 16#0000,
       SerialNo < 16#FFFF ->
    <<54:8, 00:8,  SerialNo:16/little-unsigned-integer, 00:8, 00:8>>.

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


