#!/usr/bin/env python3

import asyncio
import websockets
import json

connect_message = {
    "registerActor": {
        "name": "Sample Tagger",
        "filter": {"service": 8080},
    }
}

async def main():
    uri = "ws://localhost:10000"
    async with websockets.connect(uri) as websocket:
        await websocket.send(json.dumps(dict(id=0, payload=connect_message)))
        while True:
            resp = json.loads(await websocket.recv())["payload"]
            print(resp)
            resp = resp["newStream"]
            await websocket.send(json.dumps(dict(id=0, payload={
                "ackStream": resp["streamId"]
            })))


asyncio.get_event_loop().run_until_complete(main())