About:
==================

Template for erlang comer server using Mochiweb.

Requirements:
==================

1. Erlang
2. PHP
3. PHP Erlang Bridge http://code.google.com/p/mypeb/
4. Rebar

How to compile?
==================

1. mkdir deps
2. cd ./deps
3. git clone https://github.com/mochi/mochiweb.git
4. cd ./mochiweb
5. make
6. cd ../../
7. mkdir ebin
8. make

How to use?
==================

	./comet.sh start    - start comet server
	./comet.sh debug    - start comet server in debug mode with erlang shell
	./comet.sh stop     - stop comet server
	./comet.sh restart  - restart comet server

You need to change settings inside of comet.sh.

Examples 
==================

/http_examples
