"use strict";

exports.prettifyJson = function (json) {
    return JSON.stringify(JSON.parse(json), null, 2)
}