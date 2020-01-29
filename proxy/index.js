
var express = require('express');
var app = express();
var http = require('http').createServer(app);
var io = require('socket.io')(http);
const WebSocket = require('ws');


app.use(express.static('../frontend/dist'))


const latency = false;

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

io.on('connection', function (socket) {
    console.log('client connected');
    //const ws = new WebSocket('ws://192.168.1.194:10000/');
    //const ws = new WebSocket('ws://192.168.1.108:10000/');
    const ws = new WebSocket('ws://localhost:10000/');
    //const ws = new WebSocket('ws://172.24.182.130:10000/');

    ws.on("message", async function (s) {
        //console.log('server -> client: ' + s);
        if (latency)
            await sleep(500)
        socket.emit('msg', s);
    });

    ws.on("open", function () {
        socket.on('msg', async function (s) {
            console.log('client -> server: ' + s);
            if (latency)
                await sleep(500)
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
