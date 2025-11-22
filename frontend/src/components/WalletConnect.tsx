import React from 'react';
import { Wallet, Shield } from 'lucide-react';

interface WalletConnectProps {
  onConnect: () => void;
}

export function WalletConnect({ onConnect }: WalletConnectProps) {
  const supportedWallets = [
    { name: 'Leather', icon: '🪖' },
    { name: 'Xverse', icon: '🌐' },
    { name: 'Hiro Wallet', icon: '⚡' },
    { name: 'Stacking DAO', icon: '🏛️' },
  ];

  return (
    <div className="space-y-6">
      <button
        onClick={onConnect}
        className="w-full flex items-center justify-center px-6 py-3 border border-transparent text-base font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
      >
        <Wallet className="h-5 w-5 mr-3" />
        Connect Wallet
      </button>

      <div className="text-center">
        <p className="text-sm text-gray-600 mb-4">Supported Wallets</p>
        <div className="grid grid-cols-2 gap-3">
          {supportedWallets.map((wallet) => (
            <div
              key={wallet.name}
              className="flex items-center justify-center px-3 py-2 border border-gray-300 rounded-md bg-gray-50"
            >
              <span className="text-lg mr-2">{wallet.icon}</span>
              <span className="text-sm text-gray-700">{wallet.name}</span>
            </div>
          ))}
        </div>
      </div>

      <div className="bg-blue-50 border border-blue-200 rounded-md p-4">
        <div className="flex">
          <Shield className="h-5 w-5 text-blue-400" />
          <div className="ml-3">
            <h3 className="text-sm font-medium text-blue-800">
              Secure Oracle Network
            </h3>
            <div className="mt-2 text-sm text-blue-700">
              <p>
                AshOracle uses burn-to-submit mechanism and reputation-based consensus
                to ensure data quality and prevent manipulation.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
