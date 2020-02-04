#!/usr/bin/env python3

import asyncio
import websockets
import json

connect_message = {
    "registerPipelineNode": {
        "name": "Sample Tagger",
        "kind": "Tagger",
        "metadata_only": False,
        "filter": None,
    }
}

async def main():
    uri = "ws://localhost:10000"
    async with websockets.connect(uri) as websocket:
        await websocket.send(json.dumps(dict(id=0, payload=connect_message)))
        print("registered!")
        while True:
            resp = json.loads(await websocket.recv())["payload"]
            print(resp)
            resp = resp["pipelineStream"]
            await websocket.send(json.dumps(dict(id=0, payload={
                "pipelineResponse": [resp["id"], "neutral"]
            })))


asyncio.get_event_loop().run_until_complete(main())
