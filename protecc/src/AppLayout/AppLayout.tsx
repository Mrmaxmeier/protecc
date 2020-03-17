import React, { useState, useContext } from 'react';
import { PageHeader, Nav, NavList, NavItem, PageSidebar, Page, PageSection } from "@patternfly/react-core";
import { ConnectedIcon, DisconnectedIcon, InProgressIcon } from '@patternfly/react-icons'
import { HashRouter as Router, Route, Switch, Link } from 'react-router-dom';
import Logo from './logo.png';
import { Dashboard } from '../Dashboard/Dashboard';
import { Connected } from '../Api/ProteccApi';
import { Streams } from '../Streams/Streams';
import { Pipeline } from '../Pipeline/Pipeline';
import { Configuration } from '../Configuration/Configuration';
import { Stream } from '../Stream/Stream';


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
        { path: "/", name: "Dashboard", component: <Dashboard /> },
        { path: "/streams", name: "Streams", component: <Streams /> },
        { path: "/config", name: "Configuration", component: <Configuration /> },
        { path: "/query", name: "Query", component: <></> },
        { path: "/pipeline", name: "Taggers & Co", component: <Pipeline /> },
    ];

    let [navOpen, setNavOpen] = useState(false);
    let [page, setPage] = useState(window.location.hash);

    const header = (
        <PageHeader
            logo={<img src={Logo} width="150em" alt="Protecc" />}
            avatar={<ConnectionStatus />}
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
        <PageSidebar nav={nav} isNavOpen={navOpen} />
    );

    return (
        <Router hashType="noslash">
            <Page header={header} sidebar={sidebar}>
                <PageSection>
                    <Switch>
                        {pages.map(({ path, component }) => (
                            <Route key={path} exact path={path}>
                                {component}
                            </Route>
                        ))}
                        <Route key={"streams-port"} exact path={"/streams/port/:port"}><Streams /></Route>
                        <Route key={"streams-tag"} exact path={"/streams/tag/:tag"}><Streams /></Route>
                        <Route key={"streams-port-tag"} exact path={"/streams/:port/:tag"}><Streams /></Route>
                        <Route key={'stream'} exact path={'/stream/:id'}><Stream /></Route>
                    </Switch>
                </PageSection>
            </Page>
        </Router>
    );
}
