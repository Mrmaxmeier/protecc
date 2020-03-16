import React, { useContext } from 'react';
import { Counters } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';

export function Dashboard() {
    let counters = useContext(Counters);
    if (counters == null) return <Loading />;
    return <pre>{JSON.stringify(counters)}</pre>;
}