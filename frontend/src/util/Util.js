"use strict";

exports.prettifyJson = function (json) {
    return JSON.stringify(JSON.parse(json), null, 2)
}

exports.logoImpl = function (o) {
    return function () {
        console.log(o);
    }
}

exports.setLocalStorage = function (key) {
    return function (value) {
        return function () {
            window.localStorage.setItem(key, value);
        }
    }
}

exports.getLocalStorageImpl = function (key) {
    return function (just) {
        return function (nothing) {
            return function () {
                let v = window.localStorage.getItem(key);
                if (v == undefined || v == null)
                    return nothing;
                else
                    return just(v);
            }
        }
    }
}