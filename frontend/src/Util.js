"use strict";

exports.prettifyJson = function (json) {
    return JSON.stringify(JSON.parse(json), null, 2)
}

exports.logoImpl = function (o) {
    return function () {
        console.log(o);
    }
}