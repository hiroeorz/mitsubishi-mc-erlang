

# Module mitsubishi_mc_driver #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2018, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="types"></a>

## Data Types ##




### <a name="type-frame_type">frame_type()</a> ###


<pre><code>
frame_type() = 3E | 4E
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command-6">command/6</a></td><td>送信コマンド伝文を生成する。.</td></tr><tr><td valign="top"><a href="#get_process_identifier-1">get_process_identifier/1</a></td><td>受信伝文を受け取ってからシリアル番号を返す(4E専用)。.</td></tr><tr><td valign="top"><a href="#parse_response-3">parse_response/3</a></td><td>受信伝文バイナリをパースして返す.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="command-6"></a>

### command/6 ###

<pre><code>
command(FrameType, SerialNo, TimeoutMSec, NetworkNo, PcNo, X6::list()) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>FrameType = <a href="#type-frame_type">frame_type()</a></code></li><li><code>SerialNo = non_neg_integer()</code></li><li><code>TimeoutMSec = non_neg_integer()</code></li><li><code>NetworkNo = non_neg_integer()</code></li><li><code>PcNo = non_neg_integer()</code></li></ul>

送信コマンド伝文を生成する。

Timeoutはミリセカンドで、自局の場合は250ミリ秒 - 10000ミリ秒が望ましい.

<a name="get_process_identifier-1"></a>

### get_process_identifier/1 ###

<pre><code>
get_process_identifier(Bin::binary()) -&gt; non_neg_integer()
</code></pre>
<br />

受信伝文を受け取ってからシリアル番号を返す(4E専用)。

未テスト

<a name="parse_response-3"></a>

### parse_response/3 ###

<pre><code>
parse_response(Command, SubCommand, Bin::binary()) -&gt; ok | {ok, term()} | {error, non_neg_integer()}
</code></pre>

<ul class="definitions"><li><code>Command = non_neg_integer()</code></li><li><code>SubCommand = non_neg_integer()</code></li></ul>

受信伝文バイナリをパースして返す.

未テスト

