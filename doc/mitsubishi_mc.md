

# Module mitsubishi_mc #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

.

Copyright (c) (C) 2018, HIROE Shin

__Authors:__ HIROE Shin ([`shin@HIROE-no-MacBook-Pro.local`](mailto:shin@HIROE-no-MacBook-Pro.local)).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start_port-2">start_port/2</a></td><td>start port server.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start_port-2"></a>

### start_port/2 ###

<pre><code>
start_port(SrcIP, Port) -&gt; <a href="supervisor.md#type-startchild_ret">supervisor:startchild_ret()</a>
</code></pre>

<ul class="definitions"><li><code>SrcIP = <a href="inet.md#type-ip_address">inet:ip_address()</a></code></li><li><code>Port = <a href="inet.md#type-port_number">inet:port_number()</a></code></li></ul>

start port server.

