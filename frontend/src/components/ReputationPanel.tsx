import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { Star, TrendingUp, Award, Shield, AlertTriangle, CheckCircle } from 'lucide-react';

interface ReputationPanelProps {
  userSession: UserSession;
}

// Mock data - in real implementation, this would come from contract calls
const mockReputation = {
  totalSubmissions: 45,
  accurateSubmissions: 42,
  totalBurned: 45000000,
  reputationScore: 280,
  lastSubmission: Date.now() - 1800000,
  rank: 5,
  totalOracles: 127
};

const mockRecentSubmissions = [
  {
    id: 1,
    feedName: 'BTC/USD',
    price: 50000,
    submittedAt: Date.now() - 3600000,
    status: 'accurate',
    reward: 50000
  },
  {
    id: 2,
    feedName: 'ETH/USD',
    price: 3000,
    submittedAt: Date.now() - 7200000,
    status: 'accurate',
    reward: 30000
  },
  {
    id: 3,
    feedName: 'LINK/USD',
    price: 25,
    submittedAt: Date.now() - 10800000,
    status: 'inaccurate',
    penalty: -10000
  }
];

const mockTopOracles = [
  { address: 'SP123...', score: 320, submissions: 67 },
  { address: 'SP456...', score: 310, submissions: 58 },
  { address: 'SP789...', score: 295, submissions: 72 },
  { address: 'Your Address', score: 280, submissions: 45 },
  { address: 'SPABC...', score: 275, submissions: 63 }
];

export function ReputationPanel({ userSession }: ReputationPanelProps) {
  const [reputation, setReputation] = useState(mockReputation);
  const [recentSubmissions, setRecentSubmissions] = useState(mockRecentSubmissions);
  const [topOracles, setTopOracles] = useState(mockTopOracles);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    // In real implementation, fetch data from contract
    // fetchReputationData();
    // fetchRecentSubmissions();
    // fetchTopOracles();
  }, []);

  const getReputationLevel = (score: number) => {
    if (score >= 300) return { level: 'Elite', color: 'text-purple-600', bgColor: 'bg-purple-100' };
    if (score >= 250) return { level: 'Expert', color: 'text-blue-600', bgColor: 'bg-blue-100' };
    if (score >= 200) return { level: 'Advanced', color: 'text-green-600', bgColor: 'bg-green-100' };
    if (score >= 150) return { level: 'Intermediate', color: 'text-yellow-600', bgColor: 'bg-yellow-100' };
    return { level: 'Beginner', color: 'text-gray-600', bgColor: 'bg-gray-100' };
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'accurate': return <CheckCircle className="h-4 w-4 text-green-500" />;
      case 'inaccurate': return <AlertTriangle className="h-4 w-4 text-red-500" />;
      default: return <Clock className="h-4 w-4 text-yellow-500" />;
    }
  };

  const formatTime = (timestamp: number) => {
    const diff = Date.now() - timestamp;
    const hours = Math.floor(diff / 3600000);
    return `${hours}h ago`;
  };

  const reputationLevel = getReputationLevel(reputation.reputationScore);

  return (
    <div className="p-6">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900">Oracle Reputation</h1>
        <p className="text-gray-600 mt-1">Track your performance and standing in the oracle network</p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
        {/* Main Reputation Stats */}
        <div className="lg:col-span-2 space-y-6">
          {/* Reputation Overview */}
          <div className="bg-white shadow rounded-lg">
            <div className="px-4 py-5 sm:p-6">
              <div className="flex items-center justify-between">
                <div>
                  <h3 className="text-lg leading-6 font-medium text-gray-900">Your Reputation</h3>
                  <p className="mt-1 max-w-2xl text-sm text-gray-500">Current standing in the oracle network</p>
                </div>
                <div className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${reputationLevel.bgColor} ${reputationLevel.color}`}>
                  <Star className="h-4 w-4 mr-1" />
                  {reputationLevel.level}
                </div>
              </div>

              <div className="mt-6 grid grid-cols-2 gap-6">
                <div className="text-center">
                  <div className="text-3xl font-bold text-gray-900">{reputation.reputationScore}</div>
                  <div className="text-sm text-gray-500">Reputation Score</div>
                  <div className="mt-2 w-full bg-gray-200 rounded-full h-2">
                    <div
                      className="bg-indigo-600 h-2 rounded-full"
                      style={{ width: `${(reputation.reputationScore / 300) * 100}%` }}
                    ></div>
                  </div>
                </div>

                <div className="space-y-4">
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-gray-500">Accuracy Rate</span>
                    <span className="text-sm font-medium text-gray-900">
                      {((reputation.accurateSubmissions / reputation.totalSubmissions) * 100).toFixed(1)}%
                    </span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-gray-500">Total Submissions</span>
                    <span className="text-sm font-medium text-gray-900">{reputation.totalSubmissions}</span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-gray-500">Total Burned</span>
                    <span className="text-sm font-medium text-gray-900">{(reputation.totalBurned / 1000000).toFixed(1)} STX</span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span className="text-sm text-gray-500">Global Rank</span>
                    <span className="text-sm font-medium text-gray-900">#{reputation.rank} of {reputation.totalOracles}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>

          {/* Recent Submissions */}
          <div className="bg-white shadow rounded-lg">
            <div className="px-4 py-5 sm:px-6">
              <h3 className="text-lg leading-6 font-medium text-gray-900">Recent Submissions</h3>
              <p className="mt-1 max-w-2xl text-sm text-gray-500">Your latest oracle data submissions</p>
            </div>
            <ul role="list" className="divide-y divide-gray-200">
              {recentSubmissions.map((submission) => (
                <li key={submission.id}>
                  <div className="px-4 py-4 sm:px-6">
                    <div className="flex items-center justify-between">
                      <div className="flex items-center">
                        <div className="flex-shrink-0">
                          {getStatusIcon(submission.status)}
                        </div>
                        <div className="ml-4">
                          <div className="flex items-center">
                            <h4 className="text-sm font-medium text-gray-900">{submission.feedName}</h4>
                            <span className={`ml-2 inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
                              submission.status === 'accurate'
                                ? 'bg-green-100 text-green-800'
                                : submission.status === 'inaccurate'
                                ? 'bg-red-100 text-red-800'
                                : 'bg-yellow-100 text-yellow-800'
                            }`}>
                              {submission.status}
                            </span>
                          </div>
                          <div className="mt-1 text-sm text-gray-600">
                            Price: ${submission.price.toLocaleString()} • {formatTime(submission.submittedAt)}
                          </div>
                        </div>
                      </div>
                      <div className="text-right">
                        {submission.reward && (
                          <div className="text-sm text-green-600 font-medium">
                            +{submission.reward} rep
                          </div>
                        )}
                        {submission.penalty && (
                          <div className="text-sm text-red-600 font-medium">
                            {submission.penalty} rep
                          </div>
                        )}
                      </div>
                    </div>
                  </div>
                </li>
              ))}
            </ul>
          </div>
        </div>

        {/* Sidebar */}
        <div className="space-y-6">
          {/* Top Oracles */}
          <div className="bg-white shadow rounded-lg">
            <div className="px-4 py-5 sm:px-6">
              <h3 className="text-lg leading-6 font-medium text-gray-900">Top Oracles</h3>
              <p className="mt-1 text-sm text-gray-500">Leading reputation scores</p>
            </div>
            <ul role="list" className="divide-y divide-gray-200">
              {topOracles.map((oracle, index) => (
                <li key={oracle.address}>
                  <div className="px-4 py-4 sm:px-6">
                    <div className="flex items-center justify-between">
                      <div className="flex items-center">
                        <div className="flex-shrink-0">
                          <div className={`inline-flex items-center justify-center h-8 w-8 rounded-full ${
                            oracle.address === 'Your Address' ? 'bg-indigo-100' : 'bg-gray-100'
                          }`}>
                            <span className={`text-sm font-medium ${
                              oracle.address === 'Your Address' ? 'text-indigo-600' : 'text-gray-900'
                            }`}>
                              #{index + 1}
                            </span>
                          </div>
                        </div>
                        <div className="ml-4">
                          <div className="text-sm font-medium text-gray-900 truncate max-w-24">
                            {oracle.address}
                          </div>
                          <div className="text-sm text-gray-500">
                            {oracle.submissions} submissions
                          </div>
                        </div>
                      </div>
                      <div className="text-right">
                        <div className="text-sm font-medium text-gray-900">{oracle.score}</div>
                        <div className="text-sm text-gray-500">score</div>
                      </div>
                    </div>
                  </div>
                </li>
              ))}
            </ul>
          </div>

          {/* Reputation Benefits */}
          <div className="bg-gradient-to-r from-indigo-50 to-purple-50 border border-indigo-200 rounded-md p-4">
            <div className="flex">
              <Award className="h-5 w-5 text-indigo-400" />
              <div className="ml-3">
                <h3 className="text-sm font-medium text-indigo-800">Reputation Benefits</h3>
                <div className="mt-2 text-sm text-indigo-700">
                  <ul className="space-y-1">
                    <li className="flex items-center">
                      <span className="w-2 h-2 bg-indigo-400 rounded-full mr-2"></span>
                      Lower burn requirements
                    </li>
                    <li className="flex items-center">
                      <span className="w-2 h-2 bg-indigo-400 rounded-full mr-2"></span>
                      Higher consensus weight
                    </li>
                    <li className="flex items-center">
                      <span className="w-2 h-2 bg-indigo-400 rounded-full mr-2"></span>
                      Priority in disputes
                    </li>
                    <li className="flex items-center">
                      <span className="w-2 h-2 bg-indigo-400 rounded-full mr-2"></span>
                      Network rewards
                    </li>
                  </ul>
                </div>
              </div>
            </div>
          </div>

          {/* Reputation Levels */}
          <div className="bg-white shadow rounded-lg">
            <div className="px-4 py-5 sm:px-6">
              <h3 className="text-lg leading-6 font-medium text-gray-900">Reputation Levels</h3>
            </div>
            <div className="px-4 py-4 sm:px-6">
              <div className="space-y-3">
                {[
                  { level: 'Elite', min: 300, color: 'text-purple-600' },
                  { level: 'Expert', min: 250, color: 'text-blue-600' },
                  { level: 'Advanced', min: 200, color: 'text-green-600' },
                  { level: 'Intermediate', min: 150, color: 'text-yellow-600' },
                  { level: 'Beginner', min: 0, color: 'text-gray-600' }
                ].map((tier) => (
                  <div key={tier.level} className="flex items-center justify-between">
                    <span className={`text-sm font-medium ${tier.color}`}>{tier.level}</span>
                    <span className="text-sm text-gray-500">{tier.min}+ score</span>
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
