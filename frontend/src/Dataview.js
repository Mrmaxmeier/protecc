'use strict';

exports.hexdump = function (data) {
    console.log(data)
    let result = []
    let i = 0

    while (i < data.length) {
        let hextext = ''
        let asciitext = ''
        let start = shortToHex(i)
        for (let j = 0; j < 16; j++) {
            if (i >= data.length) break;
            if (j != 0) hextext += ' '
            hextext += byteToHex(data[i])
            asciitext += toPrintable(data[i])
            i++
        }
        result.push({
            number: start,
            hex: hextext + ' '.repeat(16 * 2 + 15 - hextext.length),
            ascii: asciitext + ' '.repeat(16 - asciitext.length)
        })
    }

    return result
}


function shortToHex(int) {
    return byteToHex((int >> 8) & 0xFF) + byteToHex(int & 0xFF)

}

function byteToHex(byte) {
    return halfByteToHex((byte >> 4) & 15) + halfByteToHex(byte & 15)
}

function halfByteToHex(halfByte) {
    return halfByte.toString(16)
}

function toPrintable(i) {
    if (i >= 32 && i <= 126)
        return String.fromCharCode(i)
    if (i == 10)
        return '↵'
    return '.'
}