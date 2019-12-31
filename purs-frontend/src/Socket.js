"use strict";

const io = require('socket.io-client');

var socket = undefined;
var listeners = {};
var closeListeners = {};

exports.get = function () {
    return socket;
}

exports.set = function (value) {
    return function () {
        socket = value;
    }
}

exports.registerListener = function (id) {
    return function (listener) {
        return function () {
            listeners[id] = listener;
        }
    }
}

exports.registerCloseListener = function (id) {
    return function (listener) {
        return function () {
            closeListeners[id] = listener;
        }
    }
}

exports.removeListener = function (id) {
    return function () {
        delete listeners[id];
        delete closeListeners[id];
    }
}

exports.getListener = function (id) {
    return function (nothing) {
        return function (just) {
            return function () {
                if (listeners[id])
                    return just(listeners[id]);
                else
                    return nothing;
            }
        }
    }
}

exports.getCloseListener = function (id) {
    return function (nothing) {
        return function (just) {
            return function () {
                if (closeListeners[id])
                    return Data_Maybe.Just.create(closeListeners[id]);
                else
                    return Data_Maybe.Nothing.value;
            }
        }
    }
}