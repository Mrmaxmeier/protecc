import React, { useContext, useState, useEffect } from 'react';
import { Api, Config } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { Record, Dictionary, Number, Static } from 'runtypes';
import { Bullseye, EmptyState, EmptyStateVariant, EmptyStateIcon, Title, Button } from '@patternfly/react-core';
import { SearchIcon } from '@patternfly/react-icons';
import { Table, TableHeader, TableBody, TableVariant } from '@patternfly/react-table';
import { ColoredLabel } from '../Components/ColoredLabel';
import { compare } from '../Util';

const IndexSizes = Record({
    indexSizes: Record({
        services: Dictionary(Number),
        tags: Dictionary(Number),
    })
})
type IndexSizes = Static<typeof IndexSizes>

export function Configuration() {
    let api = useContext(Api)
    let config = useContext(Config)
    const [indexSizes, setIndexSizes] = useState<IndexSizes | null>(null);

    useEffect(() => api.listen({ watch: 'indexSizes' }, val =>
        setIndexSizes(IndexSizes.check(val))
    ), [api]);

    if (indexSizes === null || config === null) return <Loading />;

    const columnsTags = ['Slug', 'Name', 'Owner', 'Color', 'Streams', '']
    const columnsServices = ['Slug', 'Port', 'Name', 'Streams', '']
    const rowsTags = Object.entries(config.tags)
        .sort(([a], [b]) => compare(a, b))
        .map(([id, tag]) => ({
            heightAuto: true,
            cells: [
                tag.slug,
                tag.name,
                tag.owner,
                {
                    title: <ColoredLabel useSemanticColors color={tag.color}>
                        {tag.color.toUpperCase()}
                    </ColoredLabel>
                },
                indexSizes.indexSizes.tags[id],
            ] as any
        }))

    if (Object.keys(config.tags).length === 0) {
        rowsTags.push({
            heightAuto: true,
            cells: [
                {
                    props: { colSpan: 8 },
                    title: (
                        <Bullseye>
                            <EmptyState variant={EmptyStateVariant.small}>
                                <EmptyStateIcon icon={SearchIcon} />
                                <Title headingLevel="h2" size="lg">
                                    No tags yet!
                                </Title>
                                <Button variant="link">Clear all filters</Button>
                            </EmptyState>
                        </Bullseye>
                    )
                },
            ]
        })
    }

    const rowsServices = Object.entries(config.services)
        .sort(([a], [b]) => compare(a, b))
        .map(([_, service]) => ({
            heightAuto: true,
            cells: [
                service.slug,
                service.port,
                service.name,
                indexSizes.indexSizes.services[service.port],
            ] as any
        }))


    return <>
        <Table caption="Tags" cells={columnsTags} rows={rowsTags} variant={TableVariant.compact}>
            <TableHeader />
            <TableBody />
        </Table>

        <Table caption="Services" cells={columnsServices} rows={rowsServices} variant={TableVariant.compact}>
            <TableHeader />
            <TableBody />
        </Table>
    </>;
}