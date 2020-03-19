import React, { useState, useContext } from 'react';
import { prettyPrintEndpoint, DataSegment, parseFlags } from '../Api/Types';
import { useStream, Config, serviceFromPort } from '../Api/ProteccApi';
import { Loading } from './Loading';
import { Stack, StackItem, Bullseye, Card, CardBody, OptionsMenu, OptionsMenuItem, OptionsMenuToggle, Button, SplitItem, Split, Switch } from '@patternfly/react-core';
import { Tags } from './Tags';
import { SemanticColor, ColoredLabel, Details } from './ColoredLabel';
import { DataView, DisplayType, DisplayTypes } from './DataView';
import { LightweightTable, LightweightTableHeader } from './LightweightTable';
import { toByteArray, fromByteArray } from 'base64-js';
import { formatBytes } from '../Util';

interface Props {
    streamId: number
}

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

    let details = useStream(streamId)
    let config = useContext(Config)

    let [menuOpen, setMenuOpen] = useState(false)
    let [encoding, setEncoding] = useState(DisplayTypes[0])
    let [collapse, setCollapse] = useState(true)
    let [renderLimit, setRenderLimit] = useState(100)

    if (details == null)
        return <Loading />

    const concatData = (segments: DataSegment[]) => {
        const segmentData = segments.map(segment => toByteArray(segment.data))
        let length = 0
        segmentData.forEach(data =>
            length += data.length
        )
        const data = new Uint8Array(length)
        let i = 0
        segmentData.forEach(s => {
            s.forEach((v, j) =>
                data[i + j] = v
            )
            i += s.length
        })
        return fromByteArray(data)
    }
    const data = concatData(details.segments)
    const clientData = concatData(details.segments.filter(s => s.sender === 'client'))
    const serverData = concatData(details.segments.filter(s => s.sender === 'server'))

    let headers = [
        { content: 'Id', width: 10 },
        { content: 'Server', width: 10 },
        { content: 'Client', width: 10 },
        { content: 'Data', width: 10 },
        { content: 'Server  data', width: 10 },
        { content: 'Client data', width: 10 },
        { content: 'Service', width: 10 },
        { content: 'Tags', width: 'max' },
    ]

    let segments = details.segments.filter(segment => !collapse || segment.data.length);
    let service = config === null ? null : serviceFromPort(config.services, details.server[1])

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
                                <td>
                                    <a
                                        download={details.id + '.bin'}
                                        href={'data:text/plain;base64,' + data}
                                    >
                                        {formatBytes(details.serverDataLen + details.clientDataLen)}
                                    </a>
                                </td>
                                <td>
                                    <a
                                        download={details.id + '-server.bin'}
                                        href={'data:text/plain;base64,' + serverData}
                                    >
                                        {formatBytes(details.serverDataLen)}
                                    </a>
                                </td>
                                <td>
                                    <a
                                        download={details.id + '-client.bin'}
                                        href={'data:text/plain;base64,' + clientData}
                                    >
                                        {formatBytes(details.clientDataLen)}
                                    </a>
                                </td>
                                <td>{service === null ? '-' : service.name}</td>
                                <td><Tags tags={details.tags} /></td>
                            </tr>
                        </tbody>
                    </LightweightTable>
                </StackItem>
                <StackItem>
                    <Split gutter='sm'>
                        <SplitItem>
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
                        </SplitItem>
                        <SplitItem>
                            <Bullseye>
                                <Switch label="Collapse Empty" isChecked={collapse} onChange={setCollapse} id="collapse" />
                            </Bullseye>
                        </SplitItem>
                    </Split>
                </StackItem>
                <StackItem>
                    <Stack gutter='sm'>
                        {segments.slice(0, renderLimit)
                            .map((segment, idx) =>
                                <StackItem key={idx}>
                                    <Segment segment={segment} encoding={encoding} />
                                </StackItem>
                            )}
                        {segments.length >= renderLimit ? (
                            <StackItem>
                                <Bullseye>
                                    <Button onClick={() => setRenderLimit(limit => limit + 100)}>Render moar :^)</Button>
                                </Bullseye>
                            </StackItem>
                        ) : null}
                    </Stack>
                </StackItem>
            </Stack>
        </Bullseye >
    )
}