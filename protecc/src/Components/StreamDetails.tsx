import React, { useState } from 'react';
import { StreamDetailed, prettyPrintEndpoint, DataSegment, parseFlags } from '../Api/Types';
import { useUpdatingValue } from '../Api/ProteccApi';
import { Loading } from './Loading';
import { Record, Static } from 'runtypes';
import { Stack, StackItem, Bullseye, Card, CardBody, OptionsMenu, OptionsMenuItem, OptionsMenuToggle } from '@patternfly/react-core';
import { Tags } from './Tags';
import { SemanticColor, ColoredLabel, Details } from './ColoredLabel';
import { DataView, DisplayType, DisplayTypes } from './DataView';
import { LightweightTable, LightweightTableHeader } from './LightweightTable';

interface Props {
    streamId: number
}

const DetailsUpdate = Record({
    streamDetails: StreamDetailed
})
type DetailsUpdate = Static<typeof DetailsUpdate>

function Segment({ segment, encoding }: { segment: DataSegment, encoding: DisplayType }) {
    const serverStyle = {
        borderTop: '3px solid var(--pf-global--palette--green-300)',
        marginLeft: '20%'
    }
    const clientStyle = {
        borderTop: '3px solid var(--pf-global--palette--purple-300)',
        marginRight: '20%'
    }
    const flagToColor: { [_: string]: SemanticColor | null } = {
        'fin': 'yellow',
        'syn': 'green',
        'rst': 'red',
        'psh': 'purple',
        'ack': null,
        'urg': null,
        'ece': null,
        'cwr': null
    }

    let isClient = segment.sender === 'client'
    let flags = parseFlags(segment.flags)

    return (
        <Card style={isClient ? clientStyle : serverStyle} >
            <CardBody>
                <pre style={{ textAlign: isClient ? 'left' : 'right' }}>
                    <div>
                        <ColoredLabel useSemanticColors color='grey'>SEQ <Details>{segment.seq}</Details></ColoredLabel>
                        {flags.has('ack') && <ColoredLabel useSemanticColors color='blue'>ACK <Details>{segment.ack}</Details></ColoredLabel>}
                        {Array.from(flags)
                            .map((flag) => ({ flag, color: flagToColor[flag] }))
                            .filter(({ color }) => color !== null)
                            .map(({ flag, color }) => <ColoredLabel key={flag} useSemanticColors color={color !== null ? color : 'grey'}>{flag?.toUpperCase()}</ColoredLabel>)
                        }
                    </div>
                    <DataView data={segment.data} displayType={encoding} />
                </pre>

            </CardBody>
        </Card>
    )
}

export function StreamDetails({ streamId }: Props) {

    let details = useUpdatingValue({ watch: { streamDetails: streamId } }, m => DetailsUpdate.check(m).streamDetails)
    let [menuOpen, setMenuOpen] = useState(false)
    let [encoding, setEncoding] = useState(DisplayTypes[0])

    if (details == null)
        return <Loading />

    let headers = [
        { content: 'Id', width: 10 },
        { content: 'Server', width: 10 },
        { content: 'Client', width: 10 },
        { content: 'Server  data', width: 10 },
        { content: 'Client data', width: 10 },
        { content: 'Tags', width: 'max' },
    ]

    return (
        <Bullseye>
            <Stack style={{ width: '90%' }} gutter='lg'>
                <StackItem>
                    <LightweightTable compact>
                        <LightweightTableHeader headers={headers} />
                        <tbody>
                            <tr>
                                <td>{details.id}</td>
                                <td>{prettyPrintEndpoint(details.server)}</td>
                                <td>{prettyPrintEndpoint(details.client)}</td>
                                <td>{details.serverDataLen}</td>
                                <td>{details.clientDataLen}</td>
                                <td><Tags tags={details.tags} /></td>
                            </tr>
                        </tbody>
                    </LightweightTable>
                </StackItem>
                <StackItem>
                    <OptionsMenu
                        id='encoding'
                        isOpen={menuOpen}
                        toggle={<OptionsMenuToggle onToggle={setMenuOpen} toggleTemplate={"Encoding"} />}
                        menuItems={DisplayTypes.map((type) =>
                            <OptionsMenuItem
                                key={type}
                                onSelect={() => setEncoding(type)}
                                isSelected={encoding === type}
                            >
                                {type}
                            </OptionsMenuItem>
                        )}
                    />
                </StackItem>
                <StackItem>
                    <Stack gutter='sm'>
                        {details.segments.map((segment) =>
                            <StackItem key={segment.sender + '-' + segment.seq + '-' + segment.flags + '-' + segment.timestamp}>
                                <Segment segment={segment} encoding={encoding} />
                            </StackItem>
                        )}
                    </Stack>
                </StackItem>
            </Stack>
        </Bullseye>
    )
}