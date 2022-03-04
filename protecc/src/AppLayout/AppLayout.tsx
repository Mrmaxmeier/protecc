import React, { useState, useContext } from 'react';
import { PageHeader, Nav, NavList, NavItem, PageSidebar, Page, PageSection, PageHeaderTools } from "@patternfly/react-core";
import { ConnectedIcon, DisconnectedIcon, InProgressIcon } from '@patternfly/react-icons'
import { HashRouter as Router, Route, Routes, Link } from 'react-router-dom';
import Logo from './logo.png';
import { Dashboard } from '../Dashboard/Dashboard';
import { Connected } from '../Api/ProteccApi';
import { Streams } from '../Streams/Streams';
import { Pipeline } from '../Pipeline/Pipeline';
import { Configuration } from '../Configuration/Configuration';
import { Stream } from '../Stream/Stream';
import { Query } from '../Query/Query';


function ConnectionStatus() {
    let connected = useContext(Connected);
    switch (connected) {
        case "connected":
            return <ConnectedIcon size="lg" color="green" />
        case "disconnected":
            return <DisconnectedIcon size="lg" color="red" />
        case "reconnecting":
            return <InProgressIcon size="lg" color="orange" />
    }
}


export function AppLayout() {
    let pages = [
        { path: "/", name: "Dashboard", element: <Dashboard /> },
        { path: "/streams", name: "Streams", element: <Streams /> },
        { path: "/config", name: "Configuration", element: <Configuration /> },
        { path: "/query", name: "Query", element: <Query /> },
        { path: "/pipeline", name: "Taggers & Co", element: <Pipeline /> },
    ];

    let [navOpen, setNavOpen] = useState(true);
    let [page, setPage] = useState(window.location.hash);

    const header = (
        <PageHeader
            logo={<img src={Logo} width="150em" alt="Protecc" />}
            headerTools={<PageHeaderTools>
                <ConnectionStatus />
            </PageHeaderTools>}
            showNavToggle
            isNavOpen={navOpen}
            onNavToggle={() => setNavOpen(!navOpen)}
        />
    );
    const nav = (
        <Nav onSelect={(r) => setPage(r.itemId.toString())}>
            <NavList>
                {pages.map(({ path, name }) => (
                    <NavItem key={path} itemId={path} isActive={page === path || (path !== "/" && page.startsWith(path))}>
                        <Link to={path}>{name}</Link>
                    </NavItem>
                ))}
            </NavList>
        </Nav>
    );
    const sidebar = (
        <PageSidebar nav={nav} isNavOpen={navOpen} theme='light' />
    );

    return (
        <Router>
            <Page header={header} sidebar={sidebar}>
                <PageSection>
                    <Routes>
                        {pages.map(({ path, element }) =>
                            <Route key={path} path={path} element={element} />)}
                        <Route key={"streams-port"} path={"/streams/port/:port"} element={<Streams />} />
                        <Route key={"streams-tag"} path={"/streams/tag/:tag"} element={<Streams />} />
                        <Route key={"streams-port-tag"} path={"/streams/:port/:tag"} element={<Streams />} />
                        <Route key={'stream'} path={'/stream/:id'} element={<Stream />} />
                    </Routes>
                </PageSection>
            </Page>
        </Router>
    );
}
