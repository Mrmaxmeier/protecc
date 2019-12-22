"use strict";

const io = require('socket.io-client');

exports.open = function (url) {
    return function () {
        return io(url);
    }
}

exports.onErrorImpl = function(socket) {
    return function (callback) {
        return function () {
            socket.on('error', (error) => {
                callback(error.toString())();
            });
        };
    }
}

exports.onConnectImpl = function (socket) {
    return function (callback) {
        return function () {
            socket.on('connect', () => {
                callback()();
            });
        };
    };
}

exports.onDisconnectImpl = function (socket) { 
    return function (callback) {
        return function () {
            socket.on('disconnect', (reason) => {
                callback(reason)();
            });
        };
    };
}

exports.onReconnectingImpl = function (socket) { 
    return function (callback) {
        return function () {
            socket.on('reconnecting', (attempt) => {
                callback(attempt)();
            });
        };
    };
}

exports.send = function (socket) {
    return function (s) {
        return function () {
            socket.send(s);
        }
    }
}