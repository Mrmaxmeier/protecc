#!/usr/bin/env python3

import asyncio
import websockets
import json
import base64
import struct

name = "WS-Unmask"

registerPipelineNode = {
    "registerPipelineNode": {
        "name": name,
        "kind": "mapper",
        "metadata_only": False,
        "filter": 'data_matches("Upgrade: websocket")',
    }
}


getTagID = {
    "getTagID": {
        "slug": "ws_unmasked",
        "owner": name,
        "name": "WS-Unmasked",
        "color": "olive",
    }
}


def flatten(segments, sender):
    data = b""
    for segment in segments:
        if segment["sender"] == sender:
            data += base64.b64decode(segment["data"])
    return data


def unmask_with_key(data, key):
    return bytes([x ^ key[i % 4] for i, x in enumerate(data)])


def unmask_frames(payload):
    # https://tools.ietf.org/html/rfc6455#section-5.2
    http_headers_len = payload.index(b"\r\n\r\n") + 4
    output = payload[:http_headers_len]
    payload = payload[http_headers_len:]
    while len(payload) >= 2:
        is_masked = (payload[1] & 0x80) != 0
        payload_len = payload[1] & 0x7F
        output += payload[:2]
        payload = payload[2:]
        if payload_len == 126 and len(payload) >= 2:
            payload_len = struct.unpack("!H", payload[:2])[0]
            output += payload[:2]
            payload = payload[2:]
        if payload_len == 127 and len(payload) >= 4:
            payload_len = struct.unpack("!Q", payload[:8])[0]
            output += payload[:8]
            payload = payload[8:]
        if len(payload) <= 4:
            break
        if is_masked:
            key = payload[:4]
            payload = payload[4:]
            output += b"\0\0\0\0"  # replace with non-mask
            data = payload[:payload_len]
            payload = payload[payload_len:]
            output += unmask_with_key(data, key)
        else:
            output += payload[:payload_len]
            payload = payload[payload_len:]
    output += payload  # consume remaining payload
    return output


def process(stream, tag_id):
    segments = stream["segments"]
    client_data = flatten(segments, "client")
    server_data = flatten(segments, "server")
    if (
        b"Upgrade: websocket" not in client_data
        or b"Upgrade: websocket" not in server_data
        or b"\r\n\r\n" not in client_data
        or b"\r\n\r\n" not in server_data
    ):
        return "neutral"
    client_data = unmask_frames(client_data)
    server_data = unmask_frames(server_data)
    data = {"server": server_data, "client": client_data}
    for segment in segments:
        sender = segment["sender"]
        pkt_len = len(base64.b64decode(segment["data"]))
        segment_data = data[sender]
        segment["data"] = base64.b64encode(segment_data[:pkt_len]).decode()
        data[sender] = segment_data[pkt_len:]
    print("mapped", stream["id"])
    return {"replacePayloads": {"segments": segments, "tags": [tag_id]}}


async def main():
    uri = "ws://localhost:10000"
    async with websockets.connect(uri, max_size=25 << 20) as websocket:
        await websocket.send(json.dumps(dict(id=0, payload=getTagID)))
        tag_id = json.loads(await websocket.recv())["payload"]["tagID"]
        print(f"{tag_id = :#x}")

        await websocket.send(json.dumps(dict(id=0, payload=registerPipelineNode)))
        print("registered!")
        while True:
            resp = json.loads(await websocket.recv())["payload"]
            stream = resp["pipelineStream"]
            response = {
                "id": 0,
                "payload": {
                    "pipelineResponse": [stream["id"], process(stream, tag_id)]
                },
            }
            await websocket.send(json.dumps(response))


asyncio.get_event_loop().run_until_complete(main())
