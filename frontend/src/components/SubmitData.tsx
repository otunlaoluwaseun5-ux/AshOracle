import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { Send, AlertCircle, CheckCircle, Clock } from 'lucide-react';

interface SubmitDataProps {
  userSession: UserSession;
}

// Mock data - in real implementation, this would come from contract calls
const mockFeeds = [
  { id: 1, name: 'BTC/USD', latestPrice: 50000, active: true },
  { id: 2, name: 'ETH/USD', latestPrice: 3000, active: true },
  { id: 3, name: 'LINK/USD', latestPrice: 25, active: false }
];

const mockReputation = {
  score: 280,
  minBurnAmount: 1000000 // 1 STX
};

export function SubmitData({ userSession }: SubmitDataProps) {
  const [selectedFeed, setSelectedFeed] = useState<number | null>(null);
  const [price, setPrice] = useState('');
  const [burnAmount, setBurnAmount] = useState('');
  const [loading, setLoading] = useState(false);
  const [success, setSuccess] = useState(false);
  const [feeds, setFeeds] = useState(mockFeeds);
  const [reputation, setReputation] = useState(mockReputation);

  useEffect(() => {
    // In real implementation, fetch feeds and reputation from contract
    // fetchFeeds();
    // fetchReputation();
  }, []);

  const calculateRequiredBurn = () => {
    if (!reputation.score) return mockReputation.minBurnAmount;
    // Reputation multiplier calculation
    const multiplier = reputation.score >= 200 ? 300 :
                      reputation.score >= 150 ? 200 :
                      reputation.score >= 100 ? 100 : 50;
    return (mockReputation.minBurnAmount * 100) / multiplier;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!selectedFeed || !price || !burnAmount) return;

    setLoading(true);

    try {
      // In real implementation, this would call the contract
      // const result = await submitFeedData(selectedFeed, parseInt(price), parseInt(burnAmount));

      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 2000));

      setSuccess(true);
      setTimeout(() => {
        setSuccess(false);
        setSelectedFeed(null);
        setPrice('');
        setBurnAmount('');
      }, 3000);

    } catch (error) {
      console.error('Submission failed:', error);
    } finally {
      setLoading(false);
    }
  };

  const selectedFeedData = feeds.find(f => f.id === selectedFeed);
  const requiredBurn = calculateRequiredBurn();

  return (
    <div className="p-6">
      <div className="mb-8">
        <h1 className="text-2xl font-bold text-gray-900">Submit Oracle Data</h1>
        <p className="text-gray-600 mt-1">Burn STX to submit price data and earn reputation</p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
        {/* Submission Form */}
        <div className="bg-white shadow rounded-lg">
          <div className="px-4 py-5 sm:p-6">
            <h3 className="text-lg leading-6 font-medium text-gray-900 mb-4">Submit Price Data</h3>

            <form onSubmit={handleSubmit} className="space-y-6">
              {/* Feed Selection */}
              <div>
                <label htmlFor="feed" className="block text-sm font-medium text-gray-700">
                  Select Price Feed
                </label>
                <select
                  id="feed"
                  value={selectedFeed || ''}
                  onChange={(e) => setSelectedFeed(parseInt(e.target.value))}
                  className="mt-1 block w-full pl-3 pr-10 py-2 text-base border-gray-300 focus:outline-none focus:ring-indigo-500 focus:border-indigo-500 sm:text-sm rounded-md"
                >
                  <option value="">Choose a feed...</option>
                  {feeds.filter(f => f.active).map((feed) => (
                    <option key={feed.id} value={feed.id}>
                      {feed.name} (Latest: ${feed.latestPrice?.toLocaleString()})
                    </option>
                  ))}
                </select>
              </div>

              {/* Price Input */}
              <div>
                <label htmlFor="price" className="block text-sm font-medium text-gray-700">
                  Price (USD)
                </label>
                <div className="mt-1 relative rounded-md shadow-sm">
                  <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                    <span className="text-gray-500 sm:text-sm">$</span>
                  </div>
                  <input
                    type="number"
                    id="price"
                    value={price}
                    onChange={(e) => setPrice(e.target.value)}
                    className="focus:ring-indigo-500 focus:border-indigo-500 block w-full pl-7 pr-12 sm:text-sm border-gray-300 rounded-md"
                    placeholder="0.00"
                    step="0.01"
                  />
                </div>
              </div>

              {/* Burn Amount */}
              <div>
                <label htmlFor="burnAmount" className="block text-sm font-medium text-gray-700">
                  Burn Amount (STX)
                </label>
                <div className="mt-1 relative rounded-md shadow-sm">
                  <input
                    type="number"
                    id="burnAmount"
                    value={burnAmount}
                    onChange={(e) => setBurnAmount(e.target.value)}
                    className="focus:ring-indigo-500 focus:border-indigo-500 block w-full pr-12 sm:text-sm border-gray-300 rounded-md"
                    placeholder="0.0"
                    step="0.1"
                  />
                  <div className="absolute inset-y-0 right-0 pr-3 flex items-center pointer-events-none">
                    <span className="text-gray-500 sm:text-sm">STX</span>
                  </div>
                </div>
                <p className="mt-1 text-sm text-gray-500">
                  Minimum required: {requiredBurn / 1000000} STX (based on reputation: {reputation.score})
                </p>
              </div>

              {/* Submit Button */}
              <button
                type="submit"
                disabled={loading || !selectedFeed || !price || !burnAmount || parseFloat(burnAmount) < (requiredBurn / 1000000)}
                className="w-full inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                {loading ? (
                  <>
                    <Clock className="animate-spin -ml-1 mr-3 h-5 w-5" />
                    Submitting...
                  </>
                ) : (
                  <>
                    <Send className=" -ml-1 mr-3 h-5 w-5" />
                    Submit Data
                  </>
                )}
              </button>
            </form>
          </div>
        </div>

        {/* Information Panel */}
        <div className="space-y-6">
          {/* Submission Status */}
          {success && (
            <div className="bg-green-50 border border-green-200 rounded-md p-4">
              <div className="flex">
                <CheckCircle className="h-5 w-5 text-green-400" />
                <div className="ml-3">
                  <h3 className="text-sm font-medium text-green-800">Submission Successful!</h3>
                  <div className="mt-2 text-sm text-green-700">
                    <p>Your oracle data has been submitted and is now part of the consensus calculation.</p>
                  </div>
                </div>
              </div>
            </div>
          )}

          {/* Price Deviation Warning */}
          {selectedFeedData && price && (
            <div className="bg-yellow-50 border border-yellow-200 rounded-md p-4">
              <div className="flex">
                <AlertCircle className="h-5 w-5 text-yellow-400" />
                <div className="ml-3">
                  <h3 className="text-sm font-medium text-yellow-800">Price Deviation Check</h3>
                  <div className="mt-2 text-sm text-yellow-700">
                    <p>
                      Latest price: ${selectedFeedData.latestPrice?.toLocaleString()}<br />
                      Your price: ${parseFloat(price).toLocaleString()}<br />
                      Deviation: {(((parseFloat(price) - selectedFeedData.latestPrice) / selectedFeedData.latestPrice) * 100).toFixed(2)}%
                    </p>
                  </div>
                </div>
              </div>
            </div>
          )}

          {/* How It Works */}
          <div className="bg-blue-50 border border-blue-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <AlertCircle className="h-5 w-5 text-blue-400" />
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-blue-800">How Oracle Submission Works</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <ol className="list-decimal list-inside space-y-1">
                    <li>Select a price feed to submit data for</li>
                    <li>Enter the current market price</li>
                    <li>Burn STX tokens (minimum amount based on your reputation)</li>
                    <li>Your submission is weighted by burn amount × reputation</li>
                    <li>Consensus is calculated after the submission window</li>
                    <li>Earn reputation for accurate submissions</li>
                  </ol>
                </div>
              </div>
            </div>
          </div>

          {/* Reputation Benefits */}
          <div className="bg-purple-50 border border-purple-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <AlertCircle className="h-5 w-5 text-purple-400" />
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-purple-800">Reputation Benefits</h3>
                <div className="mt-2 text-sm text-purple-700">
                  <ul className="list-disc list-inside space-y-1">
                    <li>Higher reputation = lower burn requirements</li>
                    <li>Accurate submissions increase your score</li>
                    <li>Inaccurate submissions decrease your score</li>
                    <li>Reputation affects your voting weight in consensus</li>
                    <li>Top oracles earn higher rewards</li>
                  </ul>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
