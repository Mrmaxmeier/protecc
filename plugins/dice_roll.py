#!/usr/bin/env python3

import asyncio
import websockets
import json
import random

name = "Dice Roll"
DICE_ROLL = 1337

registerPipelineNode = {
    "registerPipelineNode": {
        "name": name,
        "kind": "tagger",
        "metadata_only": False,
        "filter": None,
    }
}

getTagID = {
    "getTagID": {
        "slug": "dice_roll",
        "owner": name,
        "name": "Dice Roll",
        "color": "gray",
    }
}


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
            print(resp)
            stream = resp["pipelineStream"]
            if random.randrange(DICE_ROLL) == 0:
                await websocket.send(
                    json.dumps(
                        dict(
                            id=0,
                            payload={
                                "pipelineResponse": [
                                    stream["id"],
                                    {"tagWith": [tag_id]},
                                ]
                            },
                        )
                    )
                )
            else:
                await websocket.send(
                    json.dumps(
                        dict(
                            id=0,
                            payload={"pipelineResponse": [stream["id"], "neutral"]},
                        )
                    )
                )


asyncio.get_event_loop().run_until_complete(main())
