
var express = require('express');
var app = express();
const basicAuth = require('express-basic-auth')
var http = require('http').createServer(app);
var io = require('socket.io')(http);
const WebSocket = require('ws');

app.use(basicAuth({
    users: { 'ALLES': 'ALLES' },
    challenge: true,
    realm: 'protecc'
}))

app.use(express.static('../frontend/dist'))


const latency = false;

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}

io.on('connection', function (socket) {
    console.log('client connected');

    let ws = undefined;
    try {
        ws = new WebSocket('ws://192.168.1.194:10000/');
        // ws = new WebSocket('ws://192.168.1.108:10000/');
        // ws = new WebSocket('ws://localhost:10000/');
        // ws = new WebSocket('ws://172.24.161.112:10000/');
    } catch (e) {
        console.error("couldn't connect to server!")
        return
    }

    ws.on("message", async function (s) {
        //console.log('server -> client: ' + s);
        if (latency)
            await sleep(500)
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
        socket.on('disconnect', function (reason) {
            console.log("client disconnected");
            ws.close();
        });
        socket.on('error', function () {
            console.log("error")
        })
    });

    ws.on("close", function () {
        console.log("server disconnected");
    });

});

http.listen(4000, function () {
    console.log('listening on *:4000');
});
