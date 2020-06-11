

# Module mitsubishi_mc #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#read_dm_values-4">read_dm_values/4</a></td><td>read values from Register area.</td></tr><tr><td valign="top"><a href="#start_port-2">start_port/2</a></td><td>start port server.</td></tr><tr><td valign="top"><a href="#start_port-3">start_port/3</a></td><td></td></tr><tr><td valign="top"><a href="#write_dm_same_value-5">write_dm_same_value/5</a></td><td>write same value to Register area.</td></tr><tr><td valign="top"><a href="#write_dm_values-4">write_dm_values/4</a></td><td>write values to Register area.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="read_dm_values-4"></a>

### read_dm_values/4 ###

<pre><code>
read_dm_values(DstIP, Port, StartAddress, Count) -&gt; {ok, [non_neg_integer()]} | {error, non_neg_integer()}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li></ul>

read values from Register area.

<a name="start_port-2"></a>

### start_port/2 ###

<pre><code>
start_port(SrcIP, Port) -&gt; <a href="supervisor.md#type-startchild_ret">supervisor:startchild_ret()</a>
</code></pre>

<ul class="definitions"><li><code>SrcIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li></ul>

start port server.

<a name="start_port-3"></a>

### start_port/3 ###

<pre><code>
start_port(SrcIP, Port, FrameType) -&gt; <a href="supervisor.md#type-startchild_ret">supervisor:startchild_ret()</a>
</code></pre>

<ul class="definitions"><li><code>SrcIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>FrameType = <a href="#type-frame_type">frame_type()</a></code></li></ul>

<a name="write_dm_same_value-5"></a>

### write_dm_same_value/5 ###

<pre><code>
write_dm_same_value(DstIP, Port, StartAddress, Count, Value) -&gt; ok | {error, non_neg_integer}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>Count = non_neg_integer()</code></li><li><code>Value = non_neg_integer()</code></li></ul>

write same value to Register area.

<a name="write_dm_values-4"></a>

### write_dm_values/4 ###

<pre><code>
write_dm_values(DstIP, Port, StartAddress, List) -&gt; ok | {error, non_neg_integer()}
</code></pre>

<ul class="definitions"><li><code>DstIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li><li><code>StartAddress = non_neg_integer()</code></li><li><code>List = [non_neg_integer()]</code></li></ul>

write values to Register area.

