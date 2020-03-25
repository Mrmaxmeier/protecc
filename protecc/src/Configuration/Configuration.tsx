import React, { useContext, useState } from 'react';
import { Config, useUpdatingValue, Tag, Service, Api } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { compare, onEnter } from '../Util';
import { LightweightTableHeader, LightweightTable } from '../Components/LightweightTable';
import { Stack, StackItem, TextInput, Button, Dropdown, DropdownItem, DropdownToggle } from '@patternfly/react-core';
import { ColoredLabel, SemanticColor } from '../Components/ColoredLabel';
import { IndexSizes } from '../Api/Types';
import { EditIcon, CheckIcon, CloseIcon, PlusIcon, CaretDownIcon } from '@patternfly/react-icons';

function TextEdit({ editing, value, disabled, onChange, onSubmit }: { editing: boolean, value: string, disabled?: boolean, onChange?: (value: string) => void, onSubmit?: () => void }) {
    if (!editing)
        return <>{value}</>

    return <TextInput
        css=''
        value={value}
        onChange={onChange}
        isDisabled={disabled}
        style={{ maxWidth: '15em' }}
        onKeyDown={onEnter(onSubmit || (() => { }))}
    />
}

function ColorEdit({ editing, value, disabled, onChange }: { editing: boolean, value: SemanticColor, disabled?: boolean, onChange?: (value: SemanticColor) => void }) {
    const selected = <ColoredLabel useSemanticColors color={value}>{value.toUpperCase()}</ColoredLabel>
    const [open, setOpen] = useState(false)

    if (!editing)
        return selected

    const items = SemanticColor.alternatives.filter(v => v.value !== 'gray').map(literal =>
        <DropdownItem key={literal.value} onClick={() => { if (onChange) onChange(literal.value) }}>
            <ColoredLabel useSemanticColors color={literal.value}>{literal.value.toUpperCase()}</ColoredLabel>
        </DropdownItem>
    )
    return <Dropdown
        id='color-edit'
        isOpen={open}
        direction='down'
        toggle={<DropdownToggle onToggle={setOpen} iconComponent={CaretDownIcon}>{selected}</DropdownToggle>}
        dropdownItems={items}
    />
}

function EditButtons({ editing, onEdit, onSubmit, onCancel }: { editing: boolean, onEdit?: () => void, onSubmit?: () => void, onCancel?: () => void }) {
    if (editing)
        return <>
            <Button variant='plain' onClick={onCancel}><CloseIcon /></Button>
            <Button variant='plain' onClick={onSubmit}><CheckIcon /></Button>
        </>
    else
        return <>
            <Button variant='plain' onClick={onEdit}><EditIcon /></Button>
            <Button variant='plain' onClick={onCancel} style={{ visibility: 'hidden' }}><CloseIcon /></Button>
        </>
}

function TagRow({ tag, id, indexSize }: { id: number, tag: Tag, indexSize: number }) {
    const api = useContext(Api)
    const [edit, setEdit] = useState<Tag | null>(null)
    const editing = edit !== null
    const displayed = edit === null ? tag : edit

    const submit = () => {
        api.emit({ updateConfiguration: { setTag: edit } })
        setEdit(null)
    }

    return (
        <tr>
            <td><TextEdit editing={editing} value={displayed.slug} disabled /></td>
            <td><TextEdit editing={editing} value={displayed.name} onSubmit={submit} onChange={v => setEdit(edit === null ? null : { ...edit, name: v })} /></td>
            <td><TextEdit editing={editing} value={displayed.owner} disabled /></td>
            <td><ColorEdit editing={editing} value={displayed.color} onChange={v => setEdit({ ...tag, color: v })} /></td>
            <td>{indexSize || 0}</td>
            <td><EditButtons editing={editing} onEdit={() => setEdit(tag)} onSubmit={submit} onCancel={() => setEdit(null)} /></td>
        </tr>
    )
}

function AddTagRow() {
    const api = useContext(Api)
    const [tag, setTag] = useState<Tag>({ slug: '', name: '', color: 'grey', owner: 'webui' })

    const submit = () => {
        if (tag.slug === '' || tag.name === '')
            return
        api.emit({ updateConfiguration: { setTag: tag } })
    }

    return (
        <tr>
            <td><TextEdit editing value={tag.slug} onChange={v => setTag({ ...tag, slug: v })} onSubmit={submit} /></td>
            <td><TextEdit editing value={tag.name} onChange={v => setTag({ ...tag, name: v })} onSubmit={submit} /></td>
            <td><TextEdit editing value={tag.owner} disabled /></td>
            <td><ColorEdit editing value={tag.color} onChange={v => setTag({ ...tag, color: v })} /></td>
            <td>-</td>
            <td><Button variant='plain' onClick={submit}><PlusIcon /></Button></td>
        </tr>
    )
}

function ServiceRow({ service, id, indexSize }: { id: number, service: Service, indexSize: number }) {
    const api = useContext(Api)
    const [edit, setEdit] = useState<Service | null>(null)
    const editing = edit !== null
    const displayed = edit === null ? service : edit

    const submit = () => {
        api.emit({ updateConfiguration: { setService: edit } })
        setEdit(null)
    }

    return (
        <tr key={id}>
            <td><TextEdit editing={editing} value={displayed.slug} disabled /></td>
            <td><TextEdit editing={editing} value={displayed.port.toString()} disabled /></td>
            <td><TextEdit editing={editing} value={displayed.name} onSubmit={submit} onChange={v => setEdit(edit === null ? null : { ...edit, name: v })} /></td>
            <td>{indexSize || 0}</td>
            <td><EditButtons editing={editing} onEdit={() => setEdit(service)} onSubmit={submit} onCancel={() => setEdit(null)} /></td>
        </tr>
    )
}

function AddServiceRow({ port, indexSize }: { port: number, indexSize: number }) {
    const api = useContext(Api)
    const [service, setService] = useState<Service>({ slug: '', name: '', port: port })

    const submit = () => {
        if (service.slug === '' || service.name === '')
            return
        api.emit({ updateConfiguration: { setService: service } })
    }

    return (
        <tr>
            <td><TextEdit editing value={service.slug} onChange={v => setService({ ...service, slug: v })} onSubmit={submit} /></td>
            <td><TextEdit editing value={service.port.toString()} disabled /></td>
            <td><TextEdit editing value={service.name} onChange={v => setService({ ...service, name: v })} onSubmit={submit} /></td>
            <td>{indexSize}</td>
            <td><Button variant='plain' onClick={submit}><PlusIcon /></Button></td>
        </tr>
    )
}

export function Configuration() {
    const config = useContext(Config)
    const indexSizes = useUpdatingValue({ watch: 'indexSizes' }, m => IndexSizes.check(m).indexSizes, []);

    if (indexSizes === null || config === null) return <Loading />;

    const columnsTags = ['Slug', 'Name', 'Owner', 'Color', 'Streams', '']
    const columnsServices = ['Slug', 'Port', 'Name', 'Streams', '']

    const rowsTags = Object.entries(config.tags)
        .sort(([a], [b]) => compare(a, b))
        .map(([id, tag]) => ({
            id: parseInt(id),
            key: id,
            tag,
            indexSize: indexSizes.tags[parseInt(id)]
        }))
    const rowsServices = Object.entries(config.services)
        .sort(([a], [b]) => compare(a, b))
        .map(([id, service]) => ({
            id: parseInt(id),
            key: service.port,
            service,
            indexSize: indexSizes.services[service.port]
        }))

    const definedServices = new Set(Object.values(config.services).map(v => v.port))
    const undefinedServices = Object.entries(indexSizes.services)
        .map(([port, indexSize]) => ({ port: parseInt(port), indexSize }))
        .filter(v => !definedServices.has(v.port))
        .sort((a, b) => compare(b.indexSize, a.indexSize))


    return <Stack gutter='md'>
        <StackItem>
            <LightweightTable compact>
                <caption>Tags</caption>
                <LightweightTableHeader headers={columnsTags} />
                <tbody>
                    {rowsTags.map(p => <TagRow {...p} />)}
                    <AddTagRow />
                </tbody>
            </LightweightTable>
        </StackItem>
        <StackItem>
            <LightweightTable compact>
                <caption>Services</caption>
                <LightweightTableHeader headers={columnsServices} />
                <tbody>
                    {rowsServices.map(p => <ServiceRow key={p.service.port} {...p} />)}
                    {undefinedServices.map(p => <AddServiceRow key={p.port} {...p} />)}
                </tbody>
            </LightweightTable>
        </StackItem>
    </Stack>;
}
