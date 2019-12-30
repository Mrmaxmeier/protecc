
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

io.on('connection', function (socket) {
    console.log('a user connected');
    socket.on('test', log('test'));
    socket.on('open', log('open'));
    socket.on('close', log('close'));
    socket.on('stream', log('stream'));
});

http.listen(3000, function () {
    console.log('listening on *:3000');
});
