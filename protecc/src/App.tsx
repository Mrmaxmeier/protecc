import React from 'react';
import './App.css';
import { ApiProvider } from './Api/ProteccApi';
import { AppLayout } from './AppLayout/AppLayout';
import { StarlarkLanguageProvider } from './Monaco/StarlarkLanguageProvider';

function App() {
  return (
    <ApiProvider>
      <StarlarkLanguageProvider>
        <AppLayout />
      </StarlarkLanguageProvider>
    </ApiProvider>
  );
}

export default App;
