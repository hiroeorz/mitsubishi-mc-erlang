

# Module mitsubishi_mc_driver #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2018, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command-3">command/3</a></td><td>送信コマンド伝文を生成する。.</td></tr><tr><td valign="top"><a href="#fmt-7">fmt/7</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="command-3"></a>

### command/3 ###

<pre><code>
command(SerialNo, Timeout, X3::tuple()) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>SerialNo = non_neg_integer()</code></li><li><code>Timeout = non_neg_integer()</code></li></ul>

送信コマンド伝文を生成する。

Timeoutはミリセカンドで、自局の場合は250ミリ秒 - 10000ミリ秒が望ましい.

<a name="fmt-7"></a>

### fmt/7 ###

<pre><code>
fmt(SerialNo, NetworkNo, PcNo, UnitIONo, UnitNo, WatchTimerMSec, RequestBin) -&gt; binary()
</code></pre>

<ul class="definitions"><li><code>SerialNo = non_neg_integer()</code></li><li><code>NetworkNo = non_neg_integer()</code></li><li><code>PcNo = non_neg_integer()</code></li><li><code>UnitIONo = non_neg_integer()</code></li><li><code>UnitNo = non_neg_integer()</code></li><li><code>WatchTimerMSec = non_neg_integer()</code></li><li><code>RequestBin = binary()</code></li></ul>

