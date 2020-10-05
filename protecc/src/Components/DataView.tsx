import React from 'react';
import { toByteArray } from 'base64-js';
import { useComputed } from '../Util'

export type DisplayType = 'auto' | 'utf-8' | 'b64' | 'hexdump'
export const DisplayTypes: DisplayType[] = ['auto', 'utf-8', 'b64', 'hexdump']

interface Props {
    displayType?: DisplayType
    data: string
}

export function DataView({ displayType, data }: Props) {
    let decoded = useComputed(data, (d) => toByteArray(d))

    if (decoded === undefined)
        return <pre className="wrap">Could not decode base64: {data}</pre>

    let actualDisplayType = (!displayType || displayType === 'auto') ? auto(decoded) : displayType
    return {
        'utf-8': renderUtf8,
        b64: renderB64,
        hexdump: renderHexdump,
        auto: renderHexdump
    }[actualDisplayType](data, decoded)
}

function renderHexdump(_: string, decoded: Uint8Array) {
    let rows = hexdump(decoded)
    return (
        <pre className='scroll'>
            {rows.map((row) => (
                <span key={row.number}>
                    <span style={{ color: "#a0a000" }}>{row.number}</span>
                    {': '}
                    <span style={{ color: "#666666" }}>{row.hex}</span>
                    {'  '}
                    <span style={{ color: "#ba2121" }}>{row.ascii}</span>
                    {'\n'}
                </span>
            ))}
        </pre>
    )
}

function renderB64(data: string, _: Uint8Array) {
    return <pre className='wrap'>{data}</pre>
}

function renderUtf8(_: string, decoded: Uint8Array) {
    return <pre className='wrap'>{utf8(decoded).replace(/\n/g, '↵\n')}</pre>
}

function auto(data: Uint8Array): DisplayType {
    try {
        // FIXME: \0 is valid utf-8?
        new TextDecoder('utf-8', { fatal: true }).decode(data);
        return 'utf-8'
    } catch (e) {
        return 'hexdump'
    }
}

function utf8(data: Uint8Array): string {
    return new TextDecoder('utf-8', { fatal: false }).decode(data)
}

function hexdump(data: Uint8Array): { number: string, hex: string, ascii: string }[] {
    let result = []
    let i = 0

    while (i < data.length) {
        let hextext = ''
        let asciitext = ''
        let start = shortToHex(i)
        for (let j = 0; j < 16; j++) {
            if (i >= data.length) break;
            if (j !== 0) hextext += ' '
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


function shortToHex(int: number) {
    return byteToHex((int >> 8) & 0xFF) + byteToHex(int & 0xFF)

}

function byteToHex(byte: number) {
    return halfByteToHex((byte >> 4) & 15) + halfByteToHex(byte & 15)
}

function halfByteToHex(halfByte: number) {
    return halfByte.toString(16)
}

function toPrintable(i: number) {
    if (i >= 32 && i <= 126)
        return String.fromCharCode(i)
    if (i === 10)
        return '↵'
    return '.'
}