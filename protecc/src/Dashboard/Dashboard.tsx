import React, { useContext, useState, useEffect } from 'react';
import { Counters, Api } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { beautify } from '../Util';

export function Dashboard() {
    let api = useContext(Api)
    const [counters, setCounters] = useState<Counters | null>(null);

    useEffect(() => api.listen({ watch: 'counters' }, ({ counters }) =>
        setCounters((old) => old === null ? counters : { ...old, ...counters })
    ), [api]);

    if (counters == null) return <Loading />;
    let countersSorted: { [key: string]: number } = {}
    Object.keys(counters).sort().forEach((key) => {
        countersSorted[key] = counters[key];
    });
    return <pre>{beautify(countersSorted)}</pre>;
}