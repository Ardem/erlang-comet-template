#!/bin/bash
 
##
## usage comet.sh {start|stop|debug}
##
 
##   PA   = path to the comet server
##   ERL  = path to Erlang
##   HOSTNAME = your hostname
 
PA=/home/wwwroot/erlchat/server
ERL=/usr/local/bin/erl
HOSTNAME=app0
COMET_CONFIG=/home/wwwroot/erlchat/server/src/server.conf

case $1 in

  start)
    $ERL -pa $PA/ebin $PA/edit $PA/deps/*/ebin -boot start_sasl -sname comet@$HOSTNAME -comet_config $COMET_CONFIG -s server +K true -setcookie secret_key -detached
    echo  "Starting Webserver"
    ;;
 
  debug)
    $ERL -pa $PA/ebin $PA/edit $PA/deps/*/ebin -boot start_sasl -sname comet@$HOSTNAME -comet_config $COMET_CONFIG -s server +K true -setcookie secret_key
    ;;
 
  stop)
    $ERL -noshell -sname stop@$HOSTNAME -setcookie secret_key -eval "rpc:call('comet@$HOSTNAME',init,stop,[]), halt()."
    echo "Stopping comet server"
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
