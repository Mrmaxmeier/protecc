import React, { useContext, useState } from 'react';
import { Config } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { Stack, StackItem, Split, SplitItem, OptionsMenu, OptionsMenuToggle, OptionsMenuItem, TextInput } from '@patternfly/react-core';
import { onEnter, nanToNull } from '../Util';
import { useHistory, useParams } from 'react-router-dom';


interface Params {
    port: number | null
    tag: string | null
}

function makeUrl({ port, tag }: Params): string {
    if (port !== null && tag !== null)
        return '/streams/' + port.toString() + '/' + tag
    if (tag !== null)
        return '/streams/tag/' + tag
    if (port !== null)
        return '/streams/port/' + port.toString()
    return '/streams'
}

function Menu(props: { name: string, id: string, menuItems: any[] }) {
    let [open, setOpen] = useState(false);

    return <OptionsMenu
        isOpen={open}
        id={props.id}
        menuItems={props.menuItems}
        toggle={<OptionsMenuToggle id='toggle' onToggle={(open) => setOpen(open)} toggleTemplate={props.name} />}
    />
}

function PortMenu({ port, tag }: Params) {
    let history = useHistory();
    let config = useContext(Config);

    let selectedService = port !== null && config !== null && config.services[port] ? config.services[port] : null

    let [input, setInput] = useState((selectedService !== null || port === null) ? '' : port.toString());

    if (config === null)
        return <Loading />

    let items = [
        <TextInput
            key="inputitem"
            placeholder={'Port'}
            value={input}
            type='text'
            onChange={setInput}
            aria-label='text'
            css=''
            onKeyPress={onEnter(() => history.push(makeUrl({ port: nanToNull(parseInt(input)), tag })))}
        />,
    ];
    items.concat(Object.entries(config.services).map(([id, service]) =>
        <OptionsMenuItem
            isSelected={port === service.port}
            key={id}
            onSelect={() => history.push(makeUrl({ port: service.port, tag }))}
        >
            {service.name}
        </OptionsMenuItem>
    ))


    return <Menu name={selectedService !== null ? selectedService.name : (port === null ? "Port" : port.toString())} id='port-menu' menuItems={items} />
}

function TagMenu(params: Params) {
    let history = useHistory();
    let config = useContext(Config);

    let selectedTag = params.tag !== null && config !== null && config.services[params.tag] ? config.services[params.tag] : null

    if (config === null)
        return <Loading />


    let items = Object.entries(config.tags).map(([id, tag]) =>
        <OptionsMenuItem
            isSelected={params.tag === id}
            key={id}
            onSelect={() => history.push(makeUrl({ port: params.port, tag: id }))}
        >
            {tag.name}
        </OptionsMenuItem>
    )


    return <Menu name={selectedTag === null ? 'Tag' : selectedTag.name} id='tag-menu' menuItems={items} />
}

export function Streams() {
    let unparsedParams = useParams<{ port?: string, tag?: string }>();
    let params = {
        port: unparsedParams.port === undefined ? null : nanToNull(parseInt(unparsedParams.port)),
        tag: unparsedParams.tag === undefined ? null : unparsedParams.tag
    }

    return (
        <Stack gutter='md'>
            <StackItem>
                <Split gutter='sm'>
                    <SplitItem>
                        Port: <PortMenu {...params} />
                    </SplitItem>
                    <SplitItem>
                        Tag: <TagMenu {...params} />
                    </SplitItem>
                </Split>
            </StackItem>
            <StackItem isFilled>
                way more memes
            </StackItem>
        </Stack>
    )
}