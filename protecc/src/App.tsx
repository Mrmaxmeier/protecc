import React from 'react';
import './App.css';
import { ApiProvider } from './Api/ProteccApi';
import { AppLayout } from './AppLayout/AppLayout';


function App() {
  return (
    <ApiProvider>
      <AppLayout />
    </ApiProvider>
  );
}

export default App;
