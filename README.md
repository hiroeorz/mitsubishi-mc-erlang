MITSUBISHI MELSEC Client for Erlang/OTP Application. 

mitsubishi-mc-erlang is MESLEC command client written by Erlang.

This application support communication to MITSUBISHI PLC from Erlang application.

* MITSUBISHI ELECTRIC: <http://www.mitsubishielectric.co.jp/>
* omron PLC: <http://www.mitsubishielectric.co.jp/fa/products/cnt/plc/index.html>

Install
------------------------------------------------------------------

    $ git clone http://192.168.4.166:8080/git/shin/mitsubishi-mc-erlang.git
    $ cd mitsubishi-mc-erlang
    $ make

Example
------------------------------------------------------------------

## Start Server

    > SrcIPAddress = "192.168.0.5".
    > Port = 9600.
    > {ok, _Pid} = mitsubishi_mc:start_port(SrcIPAddress, Port).

If you use 4E frame

    > {ok, _Pid} = mitsubishi_mc:start_port(SrcIPAddress, Port, '4E').

default frame is 3E.

## Read Register Values.

    > PLCIPAddress = "192.168.0.6". %% PLC IP Address
    > Port = 9600.                  %% Port number.
    > StartAddress = 1.             %% Register Start Address
    > WordCount = 10.               %% Register Read Count
    
    > mitsubishi_mc:read_dm_values(PLCIPAddress, Port, StartAddress, WordCount).
      {ok, [0,0,0,1,2,0,0,0,0,10]}

## Write Register Values.

    > mitsubishi_mc:write_dm_values(PLCIPAddress, Port, StartAddress, [1, 2, 3]).    
      ok
