"use strict";

const io = require('socket.io-client');

var socket = undefined

exports.open = function (url) {
    return function () {
        socket = io(url);
    }
}

exports.onErrorImpl = function (callback) {
    return function () {
        socket.on('error', (error) => {
            callback(error.toString())();
        });
    };
}

exports.onConnectImpl = function (callback) {
    return function () {
        socket.on('connect', () => {
            callback()();
        });
    };
}

exports.onDisconnectImpl = function (callback) {
    return function () {
        socket.on('disconnect', (reason) => {
            callback(reason)();
        });
    };
}

exports.send = function (s) {
    return function () {
        socket.send(s);
    }
}