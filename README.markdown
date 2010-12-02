M2Haskell
=========

Tested With
-----------

  * Mongrel2/1.4-561cb4b4a4
  * 0mq 2.0.10
  * GHC 6.12.1
  * Ubuntu 10.10

Notes
-----
This server is very experimental; Please test it.

You must explicitly close connections. You can do this by sending a response with a blank body which tells Mongrel2 to close the connection.

Mongrel2 will always assume you want long-poll/keep-alive behavior until you explicitly tell it otherwise by closing the connection.

Example Instructions
--------------------

This assumes you already have mongrel2 1.4, zeromq, and ghc configured correctly. It also assumes you have the zeromq-haskell package installed (available on Hackage).

1. `mkdir run tmp logs`
2. `m2sh init`
3. `m2sh load --conf mongrel2.conf`
4. Add a host entry for "m2-dev" to the hosts file of the client you are testing
   from which points to the ip address of the mongrel2 server.
5. `m2sh start -host "m2-dev"`
6. `ghc --make Example.hs`
7. `./Example`
8. Open up a browser to http://m2-dev:6767/. You should see `hello, world!`.
9. Send Patches? Write Awesome Code? Profit?!?!

