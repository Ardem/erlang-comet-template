#!/bin/bash
 
##
## usage comet.sh {start|stop|debug}
##
 
##   PA   = path to the comet server
##   ERL  = path to Erlang
##   HOSTNAME = your hostname
##   SECRET_KEY = secret key for chat node
##   COMET_CONFIG = path to config file for comet
 
PA=/home/wwwroot/erlchat/server
ERL=/usr/local/bin/erl
HOSTNAME=localhost
SECRET_KEY=123456
COMET_CONFIG=/home/wwwroot/erlchat/server/src/server.conf

export HEART_COMMAND="/home/wwwroot/erlchat/server/comet.sh start"

start() {
    $ERL -pa $PA/ebin $PA/edit $PA/deps/*/ebin -boot start_sasl -heart -env HEART_BEAT_TIMEOUT 10 -name comet@$HOSTNAME -comet_config $COMET_CONFIG -s server +K true -setcookie $SECRET_KEY -detached
    echo  "Starting Webserver"
}

stop() {
    $ERL -noshell -name stop@$HOSTNAME -setcookie $SECRET_KEY -eval "rpc:call('comet@$HOSTNAME',init,stop,[]), halt()."
    echo "Stopping comet server"
}

debug() {
    $ERL -pa $PA/ebin $PA/edit $PA/deps/*/ebin -boot start_sasl -name comet@$HOSTNAME -comet_config $COMET_CONFIG -s server +K true -setcookie $SECRET_KEY
}


case $1 in

  start)
    start
    ;;
 
  debug)
    debug
    ;;
 
  stop)
    stop
    ;;

  restart)
    stop
    start
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug|restart}"
    exit 1
esac
 
exit 0
