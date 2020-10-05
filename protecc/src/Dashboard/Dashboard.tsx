import React, { useContext, useState, useEffect } from 'react';
import { Counters, Api, useUpdatingValue } from '../Api/ProteccApi';
import { Loading } from '../Components/Loading';
import { beautify, formatBytes, formatPercent, formatNumber, compare, formatBits } from '../Util';
import { Stack, StackItem, Bullseye, Title, Level, LevelItem } from '@patternfly/react-core';
import { ChartDonutUtilization, ChartDonut, ChartThemeColor } from '@patternfly/react-charts';
import { IndexSizes } from '../Api/Types';

export function Dashboard() {
    let api = useContext(Api)
    const indexSizes = useUpdatingValue({ watch: 'indexSizes' }, m => IndexSizes.check(m).indexSizes, []);

    const [counters, setCounters] = useState<Counters | null>(null);

    useEffect(() => api.listen({ watch: 'counters' }, ({ counters }) =>
        setCounters((old) => old === null ? counters : { ...old, ...counters })
    ), [api]);

    if (counters === null || indexSizes === null) return <Loading />;
    let countersSorted: { [key: string]: number } = {}
    Object.keys(counters).sort().forEach((key) => {
        countersSorted[key] = counters[key];
    });

    const processingTime = counters['pcap_processing_milliseconds']
    const data = counters['packet_bytes']
    const bps = processingTime && data && data / processingTime * 1000

    const serviceData = Object.entries(indexSizes.services).map(([port, count]) => ({
        x: port,
        y: count
    })).sort((a, b) => compare(a.y, b.y))
    let totalStreams = 0
    serviceData.forEach(s =>
        totalStreams += s.y
    )


    return <Stack hasGutter>
        <Bullseye>
            <StackItem style={{ width: '80%' }}>
                <Level>
                    <LevelItem>
                        <ChartDonutUtilization
                            constrainToVisibleArea
                            data={{ x: 'Processing rate out of 1Gbps', y: (bps * 8) / 1_000_000_000 * 100 }}
                            labels={({ datum }) => datum.x ? `${datum.x}: ${formatPercent(datum.y)}%` : ''}
                            subTitle='processing rate'
                            title={formatBits(bps * 8) + 'ps'}
                        />
                    </LevelItem>
                    <LevelItem>
                        <Stack>
                            <StackItem>
                                <Title headingLevel={'h1'}>{formatBytes(data)}</Title>
                            </StackItem>
                            <StackItem>
                                <Bullseye>
                                    <div style={{ fill: 'rgb(184, 187, 190)', stroke: 'transparent', fontSize: '14px' }}>Data</div>
                                </Bullseye>
                            </StackItem>
                        </Stack>
                    </LevelItem>
                    <LevelItem>
                        <ChartDonut
                            constrainToVisibleArea
                            data={serviceData}
                            labels={({ datum }) => datum.x ? `Port ${datum.x}: ${formatNumber(datum.y)}` : ''}
                            subTitle='streams'
                            themeColor={ChartThemeColor.multiOrdered}
                            title={formatNumber(totalStreams)}
                        />
                    </LevelItem>
                </Level>
            </StackItem>
        </Bullseye>
        <pre>{beautify(countersSorted)}</pre>
    </Stack>
}