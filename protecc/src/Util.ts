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

export const setIfIntOrEmpty = (cb: (v: number | null) => void) => (v: string) => {
    if (!v || v === '') {
        cb(null)
    } else {
        setIfInt(cb)(v)
    }
}

export const setIfInt = (cb: (v: number) => void) => (v: string) => {
    const n = parseInt(v)
    if (n && !isNaN(n))
        cb(n)
}

export const negate = (cb: (f: (v: boolean) => boolean) => void) => () => cb((b) => !b)

export function uniqBy<T, K>(a: T[], key: (v: T) => K): T[] {
    let seen = new Set<K>();
    return a.filter(item => {
        let k = key(item);
        return seen.has(k) ? false : seen.add(k);
    });
}