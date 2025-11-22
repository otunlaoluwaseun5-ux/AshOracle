import React from 'react';
import { Shield, Zap, Users, TrendingUp } from 'lucide-react';

interface HeaderProps {
  userData: any;
  onDisconnect: () => void;
}

export function Header({ userData, onDisconnect }: HeaderProps) {
  return (
    <header className="bg-white shadow-sm border-b border-gray-200">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between items-center h-16">
          <div className="flex items-center">
            <div className="flex-shrink-0 flex items-center">
              <Shield className="h-8 w-8 text-indigo-600" />
              <span className="ml-2 text-xl font-bold text-gray-900">AshOracle</span>
            </div>
            <div className="hidden md:block ml-10">
              <div className="flex items-baseline space-x-4">
                <div className="flex items-center text-sm text-gray-500">
                  <Zap className="h-4 w-4 mr-1" />
                  Burn-to-Submit
                </div>
                <div className="flex items-center text-sm text-gray-500">
                  <Users className="h-4 w-4 mr-1" />
                  Reputation-Based
                </div>
                <div className="flex items-center text-sm text-gray-500">
                  <TrendingUp className="h-4 w-4 mr-1" />
                  Weighted Consensus
                </div>
              </div>
            </div>
          </div>

          <div className="flex items-center space-x-4">
            <div className="text-sm text-gray-700">
              Connected: {userData?.profile?.stxAddress?.mainnet?.slice(0, 6)}...
              {userData?.profile?.stxAddress?.mainnet?.slice(-4)}
            </div>
            <button
              onClick={onDisconnect}
              className="btn btn-outline btn-sm"
            >
              Disconnect
            </button>
          </div>
        </div>
      </div>
    </header>
  );
}
