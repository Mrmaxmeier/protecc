import React from 'react';

export const onEnter = (cb: () => void) => (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') cb();
}

export const nanToNull = (n: number) => isNaN(n) ? null : n