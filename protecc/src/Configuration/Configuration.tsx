import React, { useContext, useState, useEffect } from 'react';
import { Api, Config } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { Record, Dictionary, Number, Static } from 'runtypes';
import { Bullseye, EmptyState, EmptyStateVariant, EmptyStateIcon, Title, Button } from '@patternfly/react-core';
import { SearchIcon } from '@patternfly/react-icons';
import { Table, TableHeader, TableBody, TableVariant } from '@patternfly/react-table';

let tagColors: { [key: string]: string } = {
    red: '#db2828',
    orange: '#f2711c',
    yellow: '#fbbd08',
    olive: '#b5cc18',
    green: '#21ba45',
    teal: '#00b5ad',
    blue: '#2185d0',
    violet: '#6435c9',
    purple: '#a333c8',
    pink: '#e03997',
    brown: '#a5673f',
    grey: '#767676',
    gray: '#767676',
    black: '#1b1c1d',
}

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
    let tags = config.tags
    const rowsTags = Object.keys(config.tags).sort().map(tag => ({
        heightAuto: true,
        cells: [
            tags[tag].slug,
            tags[tag].name,
            tags[tag].owner,
            {
                title: <span style={{
                    // => https://react.semantic-ui.com/elements/label/#variations-colored
                    backgroundColor: tagColors[tags[tag].color],
                    borderColor: tagColors[tags[tag].color],
                    color: '#fff',
                    display: 'inline-block',
                    lineHeight: 1,
                    verticalAlign: 'baseline',
                    margin: "0 .14285714em",
                    padding: '.5833em .833em',
                    fontWeight: 700,
                    border: '0 solid transparent',
                    borderRadius: '.28571429rem',

                    fontSize: '.8em'
                }}>
                    {tags[tag].color.toUpperCase()}
                </span>
            },
            indexSizes.indexSizes.tags[tag],
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

    let services = config.services
    const rowsServices = Object.keys(config.services).sort().map(service => ({
        heightAuto: true,
        cells: [
            services[service].slug,
            services[service].port,
            services[service].name,
            indexSizes.indexSizes.services[services[service].port],
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