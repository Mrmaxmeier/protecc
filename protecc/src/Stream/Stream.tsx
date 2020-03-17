import React from 'react';
import { useParams } from 'react-router-dom';
import { EmptyStateVariant, EmptyState, EmptyStateIcon, Title } from '@patternfly/react-core';
import { ErrorCircleOIcon } from '@patternfly/react-icons';
import { nanToNull } from '../Util';
import { StreamDetails } from '../Components/StreamDetails';

export function Stream() {
    let { id } = useParams<{ id?: string }>()
    const error = (s: string) => (
        <EmptyState variant={EmptyStateVariant.full}>
            <EmptyStateIcon icon={ErrorCircleOIcon} color={'red'} />
            <Title headingLevel='h5' size='lg'>{s}</Title>
        </EmptyState>
    )

    if (id === undefined)
        return error('Id is missing!')
    let parsed = nanToNull(parseInt(id))

    if (parsed === undefined || parsed === null)
        return error("'" + id + "' is not a valid number")

    return <StreamDetails streamId={parsed} />
}