'use strict';

let listeners = [];

exports.init = () => {
    document.onkeydown = function (evt) {
        for (let listener of listeners) {
            listener(evt.keyCode)();
        }
    }
}

exports.listen = (listener) => {
    return () => {
        listeners.push(listener);
    }
}

exports.unlisten = (listener) => {
    return () => {
        listeners = listeners.filter((l2) => l2 != listener);
    }
}