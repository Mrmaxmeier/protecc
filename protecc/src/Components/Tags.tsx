import React, { useContext, useState } from 'react';
import { ChipGroup, Chip, Dropdown, DropdownToggle, DropdownItem } from '@patternfly/react-core';
import { Config, Api } from '../Api/ProteccApi';
import { Loading } from './Loading';
import { CaretDownIcon } from '@patternfly/react-icons';
import { compare } from '../Util';
import { ColoredLabel } from './ColoredLabel';

interface Props {
    tags: number[]
    streamId?: number
}

export function Tags({ tags, streamId }: Props) {
    const config = useContext(Config)
    const api = useContext(Api)

    const [dropdownOpen, setDropdownOpen] = useState(false)

    if (config === null)
        return <Loading />

    const deleteTag = (id: number) => {
        api.emit({ removeTag: [streamId, id] })
    }
    const addTag = (id: number) => {
        console.log(id)
        api.emit({ addTag: [streamId, id] })
    }
    const editable = streamId !== undefined

    const chipGroup = <ChipGroup>
        {tags.map(tag => ({ id: tag, tag: config.tags[tag] }))
            .filter(tag => tag.tag !== undefined)
            .map((tag) =>
                <Chip key={tag.id} onClick={(e) => { deleteTag(tag.id); e.stopPropagation() }} isReadOnly={!editable}>
                    <ColoredLabel useSemanticColors color={tag.tag.color}>{tag.tag.name}</ColoredLabel>
                </Chip>
            )
        }
    </ChipGroup>

    if (!editable)
        return chipGroup

    const tagsSorted = Object.entries(config.tags).filter(([id]) => !tags.find((t) => t === parseInt(id))).sort(([a], [b]) => compare(a, b))
    const items = tagsSorted.map(([id, tag]) =>
        <DropdownItem key={id} onClick={() => addTag(parseInt(id))}>
            <ColoredLabel useSemanticColors color={tag.color}>
                {tag.name}
            </ColoredLabel>
        </DropdownItem>
    )

    return <Dropdown
        toggle={<DropdownToggle icon={CaretDownIcon} onToggle={setDropdownOpen}>{chipGroup}</DropdownToggle>}
        isOpen={dropdownOpen}
        dropdownItems={items}
        onSelect={() => setDropdownOpen(false)}
    />
}