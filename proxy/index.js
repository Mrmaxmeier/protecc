
var express = require('express');
var app = express();
const basicAuth = require('express-basic-auth')
var http = require('http').createServer(app);
var io = require('socket.io')(http);
const WebSocket = require('ws');

function randstr(length) {
    var result = '';
    var characters = '0123456789abcdef';
    for (var i = 0; i < length; i++)
        result += characters.charAt(Math.floor(Math.random() * characters.length));
    return result;
}

if (process.env.AUTH_USERNAME) {
    const password = process.env.AUTH_PASSWORD || randstr(16);
    console.log('authenticate with ' + process.env.AUTH_USERNAME + ':' + password);
    app.use(basicAuth({
        users: { [process.env.AUTH_USERNAME]: password },
        challenge: true,
        realm: 'protecc'
    }))
} else {
    console.log("authentication is disabled, don't use this in production")
}


static_dir = process.env.STATIC_DIR || '../protecc/build'
console.log('serving from ' + static_dir)
app.use(express.static(static_dir))

backend = process.env.BACKEND || 'ws://localhost:10000'
console.log('connecting to backend at ' + backend)

io.on('connection', function (socket) {
    console.log('client connected');

    let ws = undefined;
    try {
        ws = new WebSocket(backend + '/');
    } catch (e) {
        console.error("couldn't connect to server!")
        return
    }

    ws.on("message", async function (s) {
        socket.emit('msg', s);
    });

    ws.on("error", function (error) {
        console.error(error);
        socket.disconnect()
    })

    ws.on("open", function () {
        socket.on('msg', async function (s) {
            console.log('client -> server: ' + s);
            if (latency)
                await sleep(500)
            ws.send(s);
        });
        socket.on('disconnect', function () {
            console.log("client disconnected");
            ws.close();
        });
    });

    ws.on("close", function () {
        console.log("server disconnected");
    });

});

port = parseInt(process.env.PORT || '4000')

http.listen(port, function () {
    console.log('listening on *:' + port);
});
