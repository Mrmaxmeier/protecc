
var app = require('express')();
var http = require('http').createServer(app);
var io = require('socket.io')(http);

app.get('/', function (req, res) {
    res.sendFile(__dirname + '/index.html');
});

function log(what) {
    return function (arg) {
        console.log(what + ': ' + arg);
    }
}

var openStreams = {}

io.on('connection', function (socket) {
    console.log('a user connected');
    socket.on('open', function (s) {
        const arg = JSON.parse(s);
        if (arg.id in openStreams) {
            console.error("Received open for " + arg.id + " even though a stream with this id already exists!");
            return;
        }
        switch (arg.type) {
            case "counters":
                counterStream(arg.id, socket);
                break;
            default:
                console.error("Received open with unknown type " + arg.type + "!");
                break;
        }
    });
    socket.on('close', log('close'));
    socket.on('stream', log('stream'));
});

http.listen(3000, function () {
    console.log('listening on *:3000');
});

function randInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

function send(socket, id, data) {
    console.log("Sending data " + JSON.stringify(data) + " to stream " + id);
    socket.emit("stream", JSON.stringify({ id: id, arg: data }));
}

function counterStream(id, socket) {
    console.log("Opening counters stream " + id)
    var int = setInterval(function () {
        send(socket, id, { packets: randInt(1337), streams: randInt(1337), packets_unhandeled: randInt(1337) });
    }, 1000);
    socket.on("disconnect", function () {
        console.log("Counters stream " + id + " closed");
        clearInterval(int);
    });
}
