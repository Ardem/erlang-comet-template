<html>
<head>
    <title>Long polling</title>
    <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.2.6/jquery.min.js" type="text/javascript" charset="utf-8"></script>

    <script type="text/javascript" charset="utf-8">
    //var timestamp = 1343769190945083;
    var ticket = "A7BA655C66E21293F31F18C991CA5617";
    var timestamp = 0;
    //var ticket = "00";

    function addmsg(type, msg){
        $("#messages").append(
            "<div>("+type+")<br/>"+ msg +"</div>"
        );
    }

    function waitForMsg(){
        $.ajax({
            type: "GET",
            url: "/polling",
            data: { "ticket":ticket, "objects":["page", "page1"], "timestamp":timestamp },
            //data: { "ticket":ticket, "objects":[], "timestamp":timestamp },

            async: true, //If set to non-async, browser shows page as "Loading.."
            cache: false,
            timeout:60000, // Timeout in ms

            success: function(response){ // called when request to barge.php completes
                var data = eval("(" + response + ")");
                if(data.timeout==0) {
                    addmsg("new", data.message); // Add response to a .msg div (with the "new" class)
                    timestamp = data.timestamp;
                    ticket = data.ticket;
                }
                setTimeout(
                    'waitForMsg()', // Request next message
                    1000
                );
            },
            error: function(XMLHttpRequest, textStatus, errorThrown){
                addmsg("error", textStatus + " (" + errorThrown + ")");
                setTimeout(
                    'waitForMsg()', // Try again after..
                    "10000"); // milliseconds (1 second)
            },
        });
    };

    $(document).ready(function(){
        waitForMsg(); // Start the inital request
    });
    </script>
</head>
<body>
    <div id="messages">
        <div class="msg old">
            Hello
        </div>
    </div>
</body>
</html>