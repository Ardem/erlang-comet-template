<?php
set_time_limit(0);

/**
* Send message from PHP to Erlang
*
* @param string  $host
* @param integer $port
* @param integer $user_id
* @param mixed   $message
*
* @return void
*/
function sendToErlang($host, $port, $user_id, $message) {

    $post_string = 'uid='.urlencode($user_id).'&mid='.urlencode($last_insert_id).'&message='.urlencode($message);

    if ($fp = @fsockopen($host, $port, $errno, $errstr, 30))
    {
        $out = "POST ".Yii::app()->params['messageReceiver']['path']." HTTP/1.1\r\n";
        $out.= "Host: ".Yii::app()->params['messageReceiver']['host']."\r\n";
        $out.= "Content-Type: application/x-www-form-urlencoded\r\n";
        $out.= "Content-Length: ".strlen($post_string)."\r\n";
        $out.= "Connection: Close\r\n\r\n";
        if (isset($post_string)) $out.= $post_string;

        fwrite($fp, $out);
        fclose($fp);
    }
}

sendToErlang("localhost", 20010, 1, "Hello world!");
