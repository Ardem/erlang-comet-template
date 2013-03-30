<?php
set_time_limit(0);

$link = peb_connect("comet@app0", "secret_key");
if (!$link)
    echo "error:".peb_errorno()."<br>\r\nerror:".peb_error()."<br>\r\n";

$x = peb_vencode('[~a, ~i, ~i, ~s, ~p]', array(
	array('push', 35, 36, '{"id":"10256","sender_name":"John","companion_id":"35","text":"Hello world!","ts":1363473591456}', $link )
));

if (peb_send_byname("webservice",$x,$link))
	echo "send<br>\r\n";
else
	echo "error:".peb_errorno()."<br>\r\nerror:".peb_error()."<br>\r\n";

peb_close($link);

?>
