import React, { useState } from 'react';
import { AppConfig, UserSession, showConnect } from '@stacks/connect';
import { Header } from './components/Header';
import { WalletConnect } from './components/WalletConnect';
import { OracleDashboard } from './components/OracleDashboard';
import { SubmitData } from './components/SubmitData';
import { ReputationPanel } from './components/ReputationPanel';
import { AnalyticsDashboard } from './components/AnalyticsDashboard';

// Initialize Stacks app config
const appConfig = new AppConfig(['store_write', 'publish_data']);
const userSession = new UserSession({ appConfig });

function App() {
  const [userData, setUserData] = useState<any>(null);
  const [activeTab, setActiveTab] = useState('dashboard');

  const handleConnectWallet = () => {
    showConnect({
      appDetails: {
        name: 'AshOracle',
        icon: window.location.origin + '/vite.svg',
      },
      redirectTo: '/',
      onFinish: () => {
        const userData = userSession.loadUserData();
        setUserData(userData);
      },
      userSession,
    });
  };

  const handleDisconnect = () => {
    userSession.signUserOut();
    setUserData(null);
  };

  const renderContent = () => {
    if (!userData) {
      return (
        <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex items-center justify-center">
          <div className="max-w-md w-full bg-white rounded-lg shadow-lg p-8 text-center">
            <h1 className="text-3xl font-bold text-gray-900 mb-4">Welcome to AshOracle</h1>
            <p className="text-gray-600 mb-8">
              The decentralized oracle network where data providers burn STX to submit price feeds
              with reputation-based consensus.
            </p>
            <WalletConnect onConnect={handleConnectWallet} />
          </div>
        </div>
      );
    }

    return (
      <div className="min-h-screen bg-gray-50">
        <Header userData={userData} onDisconnect={handleDisconnect} />

        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          {/* Navigation Tabs */}
          <div className="mb-8">
            <nav className="flex space-x-8" aria-label="Tabs">
              {[
                { id: 'dashboard', name: 'Dashboard', icon: '📊' },
                { id: 'submit', name: 'Submit Data', icon: '📤' },
                { id: 'reputation', name: 'Reputation', icon: '⭐' },
                { id: 'analytics', name: 'Analytics', icon: '📈' },
              ].map((tab) => (
                <button
                  key={tab.id}
                  onClick={() => setActiveTab(tab.id)}
                  className={`${
                    activeTab === tab.id
                      ? 'border-indigo-500 text-indigo-600'
                      : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                  } whitespace-nowrap py-2 px-1 border-b-2 font-medium text-sm flex items-center space-x-2`}
                >
                  <span>{tab.icon}</span>
                  <span>{tab.name}</span>
                </button>
              ))}
            </nav>
          </div>

          {/* Content */}
          <div className="bg-white rounded-lg shadow">
            {activeTab === 'dashboard' && <OracleDashboard userSession={userSession} />}
            {activeTab === 'submit' && <SubmitData userSession={userSession} />}
            {activeTab === 'reputation' && <ReputationPanel userSession={userSession} />}
            {activeTab === 'analytics' && <AnalyticsDashboard userSession={userSession} />}
          </div>
        </div>
      </div>
    );
  };

  return renderContent();
}

export default App;
