import React from 'react';
import {
    Title,
    EmptyState,
    EmptyStateIcon,
    Bullseye,
} from '@patternfly/react-core';

export function Loading() {
    const Spinner = () => (
        <span className="pf-c-spinner" role="progressbar" aria-valuetext="Loading...">
            <span className="pf-c-spinner__clipper" />
            <span className="pf-c-spinner__lead-ball" />
            <span className="pf-c-spinner__tail-ball" />
        </span>
    )
    return (
        <Bullseye>
            <EmptyState>
                <EmptyStateIcon variant="container" component={Spinner} />
                <Title headingLevel="h4">
                    Loading
                </Title>
            </EmptyState>
        </Bullseye>
    );
}