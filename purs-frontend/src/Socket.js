"use strict";

const io = require('socket.io-client');

var socket = undefined;

exports.get = function () {
    return socket;
}

exports.set = function (value) {
    return function () {
        socket = value;
    }
}