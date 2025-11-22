import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { BarChart3, TrendingUp, Users, Activity, DollarSign, Shield } from 'lucide-react';

interface AnalyticsDashboardProps {
  userSession: UserSession;
}

// Mock data - in real implementation, this would come from contract calls
const mockNetworkStats = {
  totalFeeds: 15,
  activeOracles: 127,
  totalSubmissions: 15420,
  averageAccuracy: 94.2,
  totalBurned: 15420000000, // 15,420 STX
  networkUptime: 99.8
};

const mockFeedStats = [
  {
    name: 'BTC/USD',
    submissions: 2450,
    accuracy: 95.1,
    participants: 89,
    volume: 2450000000
  },
  {
    name: 'ETH/USD',
    submissions: 1890,
    accuracy: 93.8,
    participants: 76,
    volume: 1890000000
  },
  {
    name: 'LINK/USD',
    submissions: 1230,
    accuracy: 92.5,
    participants: 45,
    volume: 123000000
  },
  {
    name: 'UNI/USD',
    submissions: 980,
    accuracy: 94.7,
    participants: 38,
    volume: 98000000
  },
  {
    name: 'AAVE/USD',
    submissions: 756,
    accuracy: 96.2,
    participants: 29,
    volume: 75600000
  }
];

const mockActivityData = [
  { hour: '00', submissions: 45 },
  { hour: '04', submissions: 23 },
  { hour: '08', submissions: 89 },
  { hour: '12', submissions: 156 },
  { hour: '16', submissions: 134 },
  { hour: '20', submissions: 98 }
];

export function AnalyticsDashboard({ userSession }: AnalyticsDashboardProps) {
  const [networkStats, setNetworkStats] = useState(mockNetworkStats);
  const [feedStats, setFeedStats] = useState(mockFeedStats);
  const [activityData, setActivityData] = useState(mockActivityData);
  const [timeRange, setTimeRange] = useState('24h');

  useEffect(() => {
    // In real implementation, fetch data from contract and analytics API
    // fetchNetworkStats();
    // fetchFeedStats();
    // fetchActivityData();
  }, [timeRange]);

  return (
    <div className="p-6">
      <div className="mb-8">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold text-gray-900">Network Analytics</h1>
            <p className="text-gray-600 mt-1">Comprehensive insights into the AshOracle network</p>
          </div>
          <div className="flex space-x-2">
            {['24h', '7d', '30d'].map((range) => (
              <button
                key={range}
                onClick={() => setTimeRange(range)}
                className={`px-3 py-2 text-sm font-medium rounded-md ${
                  timeRange === range
                    ? 'bg-indigo-100 text-indigo-700'
                    : 'text-gray-500 hover:text-gray-700'
                }`}
              >
                {range}
              </button>
            ))}
          </div>
        </div>
      </div>

      {/* Network Overview Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <BarChart3 className="h-8 w-8 text-blue-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Active Feeds</dt>
                <dd className="text-lg font-medium text-gray-900">{networkStats.totalFeeds}</dd>
              </dl>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <Users className="h-8 w-8 text-green-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Active Oracles</dt>
                <dd className="text-lg font-medium text-gray-900">{networkStats.activeOracles}</dd>
              </dl>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <Activity className="h-8 w-8 text-purple-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Total Submissions</dt>
                <dd className="text-lg font-medium text-gray-900">{networkStats.totalSubmissions.toLocaleString()}</dd>
              </dl>
            </div>
          </div>
        </div>

        <div className="bg-white p-6 rounded-lg border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <Shield className="h-8 w-8 text-yellow-500" />
            </div>
            <div className="ml-5 w-0 flex-1">
              <dl>
                <dt className="text-sm font-medium text-gray-500 truncate">Avg Accuracy</dt>
                <dd className="text-lg font-medium text-gray-900">{networkStats.averageAccuracy}%</dd>
              </dl>
            </div>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
        {/* Feed Performance */}
        <div className="bg-white shadow rounded-lg">
          <div className="px-4 py-5 sm:px-6">
            <h3 className="text-lg leading-6 font-medium text-gray-900">Feed Performance</h3>
            <p className="mt-1 max-w-2xl text-sm text-gray-500">Submission volume and accuracy by price feed</p>
          </div>
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Feed
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Submissions
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Accuracy
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Participants
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {feedStats.map((feed, index) => (
                  <tr key={feed.name} className={index % 2 === 0 ? 'bg-white' : 'bg-gray-50'}>
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      {feed.name}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                      {feed.submissions.toLocaleString()}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div className="flex items-center">
                        <div className="text-sm text-gray-900">{feed.accuracy}%</div>
                        <div className="ml-2 w-16 bg-gray-200 rounded-full h-2">
                          <div
                            className="bg-green-600 h-2 rounded-full"
                            style={{ width: `${feed.accuracy}%` }}
                          ></div>
                        </div>
                      </div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                      {feed.participants}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>

        {/* Activity Chart */}
        <div className="bg-white shadow rounded-lg">
          <div className="px-4 py-5 sm:px-6">
            <h3 className="text-lg leading-6 font-medium text-gray-900">Network Activity</h3>
            <p className="mt-1 max-w-2xl text-sm text-gray-500">Submission volume over the last 24 hours</p>
          </div>
          <div className="px-4 py-4 sm:px-6">
            <div className="flex items-end space-x-2 h-64">
              {activityData.map((data, index) => (
                <div key={data.hour} className="flex-1 flex flex-col items-center">
                  <div
                    className="bg-indigo-600 rounded-t w-full mb-2"
                    style={{
                      height: `${(data.submissions / Math.max(...activityData.map(d => d.submissions))) * 200}px`,
                      minHeight: '4px'
                    }}
                  ></div>
                  <div className="text-xs text-gray-500">{data.hour}:00</div>
                  <div className="text-xs font-medium text-gray-900">{data.submissions}</div>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>

      {/* Network Health */}
      <div className="mt-8 bg-white shadow rounded-lg">
        <div className="px-4 py-5 sm:px-6">
          <h3 className="text-lg leading-6 font-medium text-gray-900">Network Health</h3>
          <p className="mt-1 max-w-2xl text-sm text-gray-500">Overall network performance and reliability metrics</p>
        </div>
        <div className="px-4 py-5 sm:p-6">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <div className="text-center">
              <div className="text-3xl font-bold text-green-600">{networkStats.networkUptime}%</div>
              <div className="text-sm text-gray-500 mt-1">Network Uptime</div>
              <div className="mt-2 w-full bg-gray-200 rounded-full h-2">
                <div
                  className="bg-green-600 h-2 rounded-full"
                  style={{ width: `${networkStats.networkUptime}%` }}
                ></div>
              </div>
            </div>

            <div className="text-center">
              <div className="text-3xl font-bold text-blue-600">
                {(networkStats.totalBurned / 1000000).toLocaleString()}
              </div>
              <div className="text-sm text-gray-500 mt-1">Total STX Burned</div>
              <div className="mt-2 flex items-center justify-center">
                <DollarSign className="h-5 w-5 text-blue-500 mr-1" />
                <span className="text-sm text-gray-600">Network Incentive</span>
              </div>
            </div>

            <div className="text-center">
              <div className="text-3xl font-bold text-purple-600">
                {((networkStats.totalSubmissions / networkStats.activeOracles)).toFixed(1)}
              </div>
              <div className="text-sm text-gray-500 mt-1">Avg Submissions/Oracle</div>
              <div className="mt-2 flex items-center justify-center">
                <TrendingUp className="h-5 w-5 text-purple-500 mr-1" />
                <span className="text-sm text-gray-600">Network Engagement</span>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Security Status */}
      <div className="mt-8 bg-green-50 border border-green-200 rounded-md p-4">
        <div className="flex">
          <Shield className="h-5 w-5 text-green-400" />
          <div className="ml-3">
            <h3 className="text-sm font-medium text-green-800">Security Status: Healthy</h3>
            <div className="mt-2 text-sm text-green-700">
              <p>
                All security mechanisms are functioning properly. Circuit breaker is inactive,
                oracle validation is enabled, and reputation slashing is operational.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
