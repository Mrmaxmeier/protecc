'use strict';

var numeral = require('numeral');

exports.formatImpl = function (i) {
    return function (format) {
        return numeral(i).format(format);
    }
}
