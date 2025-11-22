import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { TrendingUp, TrendingDown, Activity, Users, Shield, Clock } from 'lucide-react';

interface OracleDashboardProps {
  userSession: UserSession;
}

// Mock data - in real implementation, this would come from contract calls
const mockFeeds = [
  {
    id: 1,
    name: 'BTC/USD',
    price: 50000,
    change: 2.5,
    timestamp: Date.now() - 300000,
    participants: 12,
    status: 'active'
  },
  {
    id: 2,
    name: 'ETH/USD',
    price: 3000,
    change: -1.2,
    timestamp: Date.now() - 600000,
    participants: 8,
    status: 'active'
  },
  {
    id: 3,
    name: 'LINK/USD',
    price: 25,
    change: 5.8,
    timestamp: Date.now() - 900000,
    participants: 6,
    status: 'pending'
  }
];

const mockReputation = {
  totalSubmissions: 45,
  accurateSubmissions: 42,
  reputationScore: 280,
  totalBurned: 45000000,
  lastSubmission: Date.now() - 1800000
};

export function OracleDashboard({ userSession }: OracleDashboardProps) {
  const [feeds, setFeeds] = useState(mockFeeds);
  const [reputation, setReputation] = useState(mockReputation);
  const [loading, setLoading] = useState(false);

  // In real implementation, fetch data from contract
  useEffect(() => {
    // fetchContractData();
  }, []);

  const formatTime = (timestamp: number) => {
    const diff = Date.now() - timestamp;
    const minutes = Math.floor(diff / 60000);
    return `${minutes}m ago`;
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'active': return 'text-green-600 bg-green-100';
      case 'pending': return 'text-yellow-600 bg-yellow-100';
      case 'finalized': return 'text-blue-600 bg-blue-100';
      default: return 'text-gray-600 bg-gray-100';
    }
  };

  return (
    <div className="p-6">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900">Oracle Dashboard</h1>
        <p className="text-gray-600 mt-1">Monitor price feeds and your oracle reputation</p>
      </div>

      {/* Reputation Overview */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-6 mb-8">
        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <TrendingUp className="h-8 w-8 text-green-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Reputation Score</dt>
                <dd className="text-lg font-medium text-gray-900">{reputation.reputationScore}</dd>
              </dl>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <Activity className="h-8 w-8 text-blue-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Total Submissions</dt>
                <dd className="text-lg font-medium text-gray-900">{reputation.totalSubmissions}</dd>
              </dl>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <Shield className="h-8 w-8 text-purple-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Accuracy Rate</dt>
                <dd className="text-lg font-medium text-gray-900">
                  {((reputation.accurateSubmissions / reputation.totalSubmissions) * 100).toFixed(1)}%
                </dd>
              </dl>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <Clock className="h-8 w-8 text-orange-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Last Submission</dt>
                <dd className="text-lg font-medium text-gray-900">{formatTime(reputation.lastSubmission)}</dd>
              </dl>
            </div>
          </div>
        </div>
      </div>

      {/* Active Feeds */}
      <div className="bg-white shadow overflow-hidden sm:rounded-md">
        <div className="px-4 py-5 sm:px-6">
          <h3 className="text-lg leading-6 font-medium text-gray-900">Active Price Feeds</h3>
          <p className="mt-1 max-w-2xl text-sm text-gray-500">Current oracle data feeds and their status</p>
        </div>
        <ul role="list" className="divide-y divide-gray-200">
          {feeds.map((feed) => (
            <li key={feed.id}>
              <div className="px-4 py-4 sm:px-6">
                <div className="flex items-center justify-between">
                  <div className="flex items-center">
                    <div className="flex-shrink-0">
                      <div className="h-10 w-10 rounded-full bg-gray-100 flex items-center justify-center">
                        <span className="text-sm font-medium text-gray-900">
                          {feed.name.split('/')[0][0]}{feed.name.split('/')[1][0]}
                        </span>
                      </div>
                    </div>
                    <div className="ml-4">
                      <div className="flex items-center">
                        <h4 className="text-sm font-medium text-gray-900">{feed.name}</h4>
                        <span className={`ml-2 inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${getStatusColor(feed.status)}`}>
                          {feed.status}
                        </span>
                      </div>
                      <div className="flex items-center mt-1">
                        <span className="text-lg font-semibold text-gray-900">${feed.price.toLocaleString()}</span>
                        <span className={`ml-2 flex items-center text-sm ${feed.change >= 0 ? 'text-green-600' : 'text-red-600'}`}>
                          {feed.change >= 0 ? <TrendingUp className="h-4 w-4 mr-1" /> : <TrendingDown className="h-4 w-4 mr-1" />}
                          {Math.abs(feed.change)}%
                        </span>
                      </div>
                    </div>
                  </div>
                  <div className="flex items-center space-x-4">
                    <div className="text-right">
                      <div className="text-sm text-gray-500">Participants</div>
                      <div className="flex items-center text-sm text-gray-900">
                        <Users className="h-4 w-4 mr-1" />
                        {feed.participants}
                      </div>
                    </div>
                    <div className="text-right">
                      <div className="text-sm text-gray-500">Updated</div>
                      <div className="text-sm text-gray-900">{formatTime(feed.timestamp)}</div>
                    </div>
                  </div>
                </div>
              </div>
            </li>
          ))}
        </ul>
      </div>
    </div>
  );
}
