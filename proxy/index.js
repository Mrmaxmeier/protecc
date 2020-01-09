
var app = require('express')();
var http = require('http').createServer(app);
var io = require('socket.io')(http);
const WebSocket = require('ws');

app.get('/', function (req, res) {
    res.sendFile(__dirname + '/index.html');
});


io.on('connection', function (socket) {
    console.log('client connected');
    const ws = new WebSocket('ws://192.168.1.194:10000/');

    ws.on("message", function (s) {
        console.log('server -> client: ' + s);
        socket.emit('msg', s);
    });

    ws.on("open", function () {
        socket.on('msg', function (s) {
            console.log('client -> server: ' + s);
            ws.send(s);
        });
        socket.on('disconnect', function (reason) {
            console.log("client disconnected");
            ws.close();
        });
    });

    ws.on("close", function () {
        console.log("server disconnected");
        socket.disconnect();
    });

});

http.listen(4000, function () {
    console.log('listening on *:4000');
});
