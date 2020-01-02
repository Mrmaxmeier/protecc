
var app = require('express')();
var http = require('http').createServer(app);
var io = require('socket.io')(http);
const WebSocket = require('ws');

const ws = new WebSocket('ws://[::1]:10000/');

app.get('/', function (req, res) {
    res.sendFile(__dirname + '/index.html');
});

var connections = {};

io.on('connection', function (socket) {
    console.log('client connected');

    const send = function (arg) {
        socket.emit('stream', JSON.stringify({ id: arg.id, data: arg.payload }));
    }

    socket.on('open', function (s) {
        const arg = JSON.parse(s);
        console.log('open: ' + s);
        ws.send(JSON.stringify({ id: arg.id, payload: { Watch: arg.type } }));
        connections[arg.id] = send;
    });
    socket.on('stream', function (s) {
        const arg = JSON.parse(s);
        console.log('stream: ' + s);
        ws.send(JSON.stringify({ id: arg.id, payload: arg.data }));
    });
    socket.on('close', function (s) {
        const arg = JSON.parse(s);
        console.log('close: ' + s);
        ws.send(JSON.stringify({ id: arg.id, payload: "Cancel" }));
        delete connections[arg.id];
    });
});

ws.on("message", function (s) {
    console.log('message: ' + s);
    const arg = JSON.parse(s);
    connections[arg.id](arg);
});

http.listen(3000, function () {
    console.log('listening on *:3000');
});
