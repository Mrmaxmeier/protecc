'use strict';

let config = undefined;
let listeners = [];

exports.getImpl = function (just) {
    return function (nothing) {
        return function () {
            if (config === undefined)
                return nothing;
            else
                return just(config);
        }
    }
}

exports.setImpl = function (c) {
    return function () {
        config = c;
        for (let listener of listeners) {
            listener(config)();
        }
    }
}

exports.listen = function (listener) {
    return function () {
        listeners.push(listener);
    }
};

exports.unlisten = function (listener) {
    return function () {
        listeners = listeners.filter(function (v, i, a) {
            return v != listener;
        });
    }
}