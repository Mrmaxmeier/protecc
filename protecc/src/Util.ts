import React, { useState } from 'react';

export const onEnter = (cb: () => void) => (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') cb();
}

export const nanToNull = (n: number) => isNaN(n) ? null : n

export function useComputed<T, S>(value: T, computation: (t: T) => S): S {
    let [cache, setCache] = useState(() => ({ value, computed: computation(value) }))
    if (value === cache.value)
        return cache.computed

    let computed = computation(value)
    setCache({ value, computed })
    return computed
}

export function compare<T>(a: T, b: T): number {
    if (a > b)
        return 1
    if (a === b)
        return 0
    return -1
}