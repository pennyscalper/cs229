#!/usr/bin/env python

import sys, os, math, random, collections 
import pandas as pd
import numpy as np

####################################################################################################
#                                               MDP                                             
####################################################################################################

class MDP:
    # Return the start state.
    def startState(self): raise NotImplementedError("Override me")

    # Return set of actions possible from |state|.
    def actions(self, state): raise NotImplementedError("Override me")

    def succAndProbReward(self, state, action): raise NotImplementedError("Override me")

    def discount(self): raise NotImplementedError("Override me")


# StockReturnPathMDP
# contains helper functions to: 1) load intraday stock features
#                               2) simulate intraday return path 
#                               3) initialize a day with 930am starting
# contains MDP for collecting rewards on state transition based on any action

# INPUT:
# ticker - used to form the full file path containing features
# dataDir - contains data
# discount
# port - initial portfolio value at the start of any intraday iteration
# transactionCost - cost to trade
# eod - end of day, 390th minute
# randomize - if iterations are randomized to avoid agent learn same set of data in continuation
# startYear/endYear - control to keep the simulation within certain dates so that data loading is done once 
#                     and the agent can be run with different params for different dates using the same mdp

class StockReturnPathMDP(MDP):
    def __init__(self, ticker, dataDir, port=100, transactCost=0 * 1e-4, eod=390, randomize=False, startYear=2003, endYear=2017):
        self.ticker = ticker 
        self.dataDir = dataDir 
        self.transactCost = transactCost
        self.eod = eod 
        self.port = port 

        # start/end year control the simulation period
        self.startYear = startYear
        self.endYear = endYear
        featuresFile = dataDir + '/' + 'features_5Mins_' + ticker + '.csv'
        print('Loading features file...',featuresFile)
        self.allData = pd.read_csv(featuresFile)  
        self.stockStates = collections.defaultdict() 
        self.computeStockStates()
        self.dateList = sorted(self.stockStates.keys())

        # for tracking current stock state within a simulation(or a simulate iteration)
        self.simRandomize = randomize 
        self.simDateCounter = 0
        self.simCurrentDate = None 
        self.simStockStates = None
        self.nextSimTrial()

    # helpers for MDP states and states data generations
    def computeStockStates(self):
        print('extracting all intraday stock states between %d to %d...' % (self.startYear, self.endYear))
        df = self.allData[(self.allData.year >= self.startYear) & (self.allData.year <= self.endYear)]
        self.stockStates = collections.defaultdict() 
        for date, dfSubset in df.groupby('date'):
            self.stockStates[date] = []
            for i in range(len(dfSubset.index)): 
                self.stockStates[date].append(dfSubset.iloc[i, 2:len(df.columns)].T.to_dict())
        print('ready to simulate!!!')

    def setSimRandomization(self, randomize):
        self.simRandomize = randomize 

    def updateSimData(self, startYear, endYear):
        self.startYear = startYear
        self.endYear = endYear
        self.computeStockStates()
        self.dateList = sorted(self.stockStates.keys())
        self.reset()

    def reset(self):
        self.simDateCounter = 0
        self.simCurrentDate = None
        self.simStockStates = None
        self.nextSimTrial()

    # set up the stockStates for a sim trial and the MDP is ready to go  
    def nextSimTrial(self):
        if not self.canSimulate():
            print('exhausted all data!!! you can rerun by running reset()')

        if self.simRandomize:
            self.simCurrentDate = random.sample(self.dateList, 1)[0]
        else: 
            self.simCurrentDate = self.dateList[self.simDateCounter] 
        self.simStockStates = self.stockStates[self.simCurrentDate]

        print('simulating mdp for date:', self.simCurrentDate)
        self.simDateCounter += 1
    
    def canSimulate(self):
        return self.simRandomize or (self.simDateCounter < len(self.dateList) and self.simCurrentDate != self.dateList[-1])

    # MDP
    def startState(self):
        #portfolio value, bin index, stock state at bin index
        return (self.port, 1, self.simStockStates[0])

    def isEnd(self, state):
        return state[2]['bin'] == self.eod
    
    def actions(self, state):
        return ['Buy', 'Sell', 'Hold']

    # list of (newState, prob, reward)
    def succAndProbReward(self, state, action):
        if self.isEnd(state):
            return []
        
        currPort = state[0]
        nextBinIdx = state[1] + 1
        nextStockState = self.simStockStates[nextBinIdx - 1]

        if action == 'Hold':
            reward = 0
        elif action == 'Sell':
            reward = - currPort * state[2]['y']
        else:
            reward = currPort * state[2]['y']

        return [((currPort + reward, nextBinIdx, nextStockState), 1, reward)]

    def discount(self):
        return 1

# MDP to trade stock and sector both simultaneously
class PortReturnPathMDP(MDP):
    def __init__(self, ticker, dataDir, port=100, transactCost=0 * 1e-4, eod=390, randomize=False, startYear=2003, endYear=2017):
        self.ticker = ticker 
        self.dataDir = dataDir 
        self.transactCost = transactCost
        self.eod = eod 
        self.port = port 

        # start/end year control the simulation period
        self.startYear = startYear
        self.endYear = endYear
        featuresFile = dataDir + '/' + 'features_5Mins_' + ticker + '.csv'
        print('Loading features file...',featuresFile)
        self.allData = pd.read_csv(featuresFile)  
        self.stockStates = collections.defaultdict() 
        self.computeStockStates()
        self.dateList = sorted(self.stockStates.keys())

        # for tracking current stock state within a simulation(or a simulate iteration)
        self.simRandomize = randomize 
        self.simDateCounter = 0
        self.simCurrentDate = None 
        self.simStockStates = None
        self.nextSimTrial()

    # helpers for MDP states and states data generations
    def computeStockStates(self):
        print('extracting all intraday stock states between %d to %d...' % (self.startYear, self.endYear))
        df = self.allData[(self.allData.year >= self.startYear) & (self.allData.year <= self.endYear)]
        self.stockStates = collections.defaultdict() 
        for date, dfSubset in df.groupby('date'):
            self.stockStates[date] = []
            for i in range(len(dfSubset.index)): 
                self.stockStates[date].append(dfSubset.iloc[i, 2:len(df.columns)].T.to_dict())
        print('ready to simulate!!!')

    def setSimRandomization(self, randomize):
        self.simRandomize = randomize 

    def updateSimData(self, startYear, endYear):
        self.startYear = startYear
        self.endYear = endYear
        self.computeStockStates()
        self.dateList = sorted(self.stockStates.keys())
        self.reset()

    def reset(self):
        self.simDateCounter = 0
        self.simCurrentDate = None
        self.simStockStates = None
        self.nextSimTrial()

    # set up the stockStates for a sim trial and the MDP is ready to go  
    def nextSimTrial(self):
        if not self.canSimulate():
            print('exhausted all data!!! you can rerun by running reset()')

        if self.simRandomize:
            self.simCurrentDate = random.sample(self.dateList, 1)[0]
        else: 
            self.simCurrentDate = self.dateList[self.simDateCounter] 
        self.simStockStates = self.stockStates[self.simCurrentDate]

        print('simulating mdp for date:', self.simCurrentDate)
        self.simDateCounter += 1
    
    def canSimulate(self):
        return self.simRandomize or (self.simDateCounter < len(self.dateList) and self.simCurrentDate != self.dateList[-1])

    # MDP
    def startState(self):
        #portfolio value, bin index, stock state at bin index
        return (self.port, 1, self.simStockStates[0])

    def isEnd(self, state):
        return state[2]['bin'] == self.eod
    
    def actions(self, state):
        return ['BuyBuy', 'BuySell', 'BuyHold', 'SellBuy', 'SellSell', 'SellHold', 'HoldBuy', 'HoldSell', 'HoldHold']

    # list of (newState, prob, reward)
    def succAndProbReward(self, state, action):
        if self.isEnd(state):
            return []
        
        currPort = state[0]
        nextBinIdx = state[1] + 1
        nextStockState = self.simStockStates[nextBinIdx - 1]

        if action == 'BuyBuy':
            reward = 0.5 * currPort * state[2]['y'] + 0.5 * currPort * state[2]['y.xlk']
        elif action == 'BuySell':
            reward = currPort * state[2]['y'] - currPort * state[2]['y.xlk']
        elif action == 'BuyHold':
            reward = currPort * state[2]['y']
        elif action == 'SellBuy':
            reward = - currPort * state[2]['y'] + currPort * state[2]['y.xlk']
        elif action == 'SellSell':
            reward = - 0.5 * currPort * state[2]['y'] - 0.5 * currPort * state[2]['y.xlk']
        elif action == 'SellHold':
            reward = - currPort * state[2]['y']
        elif action == 'HoldBuy':
            reward = currPort * state[2]['y.xlk']
        elif action == 'HoldSell':
            reward = - currPort * state[2]['y.xlk']
        else:
            reward = 0

        return [((currPort + reward, nextBinIdx, nextStockState), 1, reward)]

    def discount(self):
        return 1

class PortReturnPathMDP1(MDP):
    def __init__(self, ticker, dataDir, port=100, transactCost=0 * 1e-4, eod=390, randomize=False, startYear=2003, endYear=2017):
        self.ticker = ticker 
        self.dataDir = dataDir 
        self.transactCost = transactCost
        self.eod = eod 
        self.port = port 

        # start/end year control the simulation period
        self.startYear = startYear
        self.endYear = endYear
        featuresFile = dataDir + '/' + 'features_5Mins_' + ticker + '.csv'
        print('Loading features file...',featuresFile)
        self.allData = pd.read_csv(featuresFile)  
        self.stockStates = collections.defaultdict() 
        self.computeStockStates()
        self.dateList = sorted(self.stockStates.keys())

        # for tracking current stock state within a simulation(or a simulate iteration)
        self.simRandomize = randomize 
        self.simDateCounter = 0
        self.simCurrentDate = None 
        self.simStockStates = None
        self.nextSimTrial()

    # helpers for MDP states and states data generations
    def computeStockStates(self):
        print('extracting all intraday stock states between %d to %d...' % (self.startYear, self.endYear))
        df = self.allData[(self.allData.year >= self.startYear) & (self.allData.year <= self.endYear)]
        self.stockStates = collections.defaultdict() 
        for date, dfSubset in df.groupby('date'):
            self.stockStates[date] = []
            for i in range(len(dfSubset.index)): 
                self.stockStates[date].append(dfSubset.iloc[i, 2:len(df.columns)].T.to_dict())
        print('ready to simulate!!!')

    def setSimRandomization(self, randomize):
        self.simRandomize = randomize 

    def updateSimData(self, startYear, endYear):
        self.startYear = startYear
        self.endYear = endYear
        self.computeStockStates()
        self.dateList = sorted(self.stockStates.keys())
        self.reset()

    def reset(self):
        self.simDateCounter = 0
        self.simCurrentDate = None
        self.simStockStates = None
        self.nextSimTrial()

    # set up the stockStates for a sim trial and the MDP is ready to go  
    def nextSimTrial(self):
        if not self.canSimulate():
            print('exhausted all data!!! you can rerun by running reset()')

        if self.simRandomize:
            self.simCurrentDate = random.sample(self.dateList, 1)[0]
        else: 
            self.simCurrentDate = self.dateList[self.simDateCounter] 
        self.simStockStates = self.stockStates[self.simCurrentDate]

        print('simulating mdp for date:', self.simCurrentDate)
        self.simDateCounter += 1
    
    def canSimulate(self):
        return self.simRandomize or (self.simDateCounter < len(self.dateList) and self.simCurrentDate != self.dateList[-1])

    # MDP
    def startState(self):
        #portfolio value, bin index, stock state at bin index
        return (self.port, 1, self.simStockStates[0])

    def isEnd(self, state):
        return state[2]['bin'] == self.eod
    
    def actions(self, state):
        allActions = ['BuyBuyHold', 'BuySellHold', 'BuyHoldHold', 'SellBuyHold', 'SellSellHold', 'SellHoldHold', 'HoldBuyHold', 'HoldSellHold', 'HoldHoldHold', \
                      'BuyBuyBuy', 'BuySellBuy', 'BuyHoldBuy', 'SellBuyBuy', 'SellSellBuy', 'SellHoldBuy', 'HoldBuyBuy', 'HoldSellBuy', 'HoldHoldBuy', \
                      'BuyBuySell', 'BuySellSell', 'BuyHoldSell', 'SellBuySell', 'SellSellSell', 'SellHoldSell', 'HoldBuySell', 'HoldSellSell', 'HoldHold']
        return allActions

    # list of (newState, prob, reward)
    def succAndProbReward(self, state, action):
        if self.isEnd(state):
            return []
        
        currPort = state[0]
        nextBinIdx = state[1] + 1
        nextStockState = self.simStockStates[nextBinIdx - 1]

        if action == 'BuyBuyHold':
            reward = 0.5 * currPort * state[2]['y'] + 0.5 * currPort * state[2]['y.xlk']
        elif action == 'BuySellHold':
            reward = currPort * state[2]['y'] - currPort * state[2]['y.xlk']
        elif action == 'BuyHoldHold':
            reward = currPort * state[2]['y']
        elif action == 'SellBuyHold':
            reward = - currPort * state[2]['y'] + currPort * state[2]['y.xlk']
        elif action == 'SellSellHold':
            reward = - 0.5 * currPort * state[2]['y'] - 0.5 * currPort * state[2]['y.xlk']
        elif action == 'SellHoldHold':
            reward = - currPort * state[2]['y']
        elif action == 'HoldBuyHold':
            reward = currPort * state[2]['y.xlk']
        elif action == 'HoldSellHold':
            reward = - currPort * state[2]['y.xlk']

        elif action == 'BuyBuyBuy':
            reward = 0.4 * currPort * state[2]['y'] + 0.3 * currPort * state[2]['y.xlk'] + 0.3 * currPort * state[2]['y.spy']
        elif action == 'BuySellBuy':
            reward = 0.5 * currPort * state[2]['y'] - currPort * state[2]['y.xlk'] + 0.5 * currPort * state[2]['y.spy']
        elif action == 'BuyHoldBuy':
            reward = 0.5 * currPort * state[2]['y'] + 0.5 * currPort * state[2]['y.spy']
        elif action == 'SellBuyBuy':
            reward = - currPort * state[2]['y'] + 0.5 * currPort * state[2]['y.xlk'] + 0.5 * currPort * state[2]['y.spy']
        elif action == 'SellSellBuy':
            reward = - 0.5 * currPort * state[2]['y'] - 0.5 * currPort * state[2]['y.xlk'] + currPort * state[2]['y.spy']
        elif action == 'SellHoldBuy':
            reward = - currPort * state[2]['y'] + currPort * state[2]['y.spy']
        elif action == 'HoldBuyBuy':
            reward = 0.5 * currPort * state[2]['y.xlk'] + 0.5 * currPort * state[2]['y.spy']
        elif action == 'HoldSellBuy':
            reward = - currPort * state[2]['y.xlk'] + currPort * state[2]['y.spy']

        elif action == 'BuyBuySell':
            reward = 0.5 * currPort * state[2]['y'] + 0.5 * currPort * state[2]['y.xlk'] - currPort * state[2]['y.spy']
        elif action == 'BuySellSell':
            reward = currPort * state[2]['y'] - 0.5 * currPort * state[2]['y.xlk'] - 0.5 * currPort * state[2]['y.spy']
        elif action == 'BuyHoldSell':
            reward = currPort * state[2]['y'] - currPort * state[2]['y.spy']
        elif action == 'SellBuySell':
            reward = - 0.5 * currPort * state[2]['y'] + currPort * state[2]['y.xlk'] - 0.5 * currPort * state[2]['y.spy']
        elif action == 'SellSellSell':
            reward = - 0.4 * currPort * state[2]['y'] - 0.3 * currPort * state[2]['y.xlk'] - 0.3 * currPort * state[2]['y.spy']
        elif action == 'SellHoldSell':
            reward = - 0.5 * currPort * state[2]['y'] - 0.5 * currPort * state[2]['y.spy']
        elif action == 'HoldBuySell':
            reward = currPort * state[2]['y.xlk'] - currPort * state[2]['y.spy']
        elif action == 'HoldSellSell':
            reward = - 0.5 * currPort * state[2]['y.xlk'] - 0.5 * currPort * state[2]['y.spy']
        else:
            reward = 0

        return [((currPort + reward, nextBinIdx, nextStockState), 1, reward)]

    def discount(self):
        return 1
####################################################################################################
#                               Feature Extractors                                             
####################################################################################################

# linear feature extractor
def featureExtractorSimple(state, action):
    port, binIdx, stockState = state
    
    featuresKey = ['binReturnNorm','return3Norm','return12Norm','returnSODNorm','prevDayReturnNorm',
                    'binReturnNorm.xlk','return3Norm.xlk','return12Norm.xlk','returnSODNorm.xlk','prevDayReturnNorm.xlk',
                    'binReturnNorm.spy','return3Norm.spy','return12Norm.spy','returnSODNorm.spy','prevDayReturnNorm.spy']
    features =[]
    for f in featuresKey: 
        #features.append(((action, f), 1 if stockState[f] > 0 else -1))
        features.append(((action, f), sigmoid(stockState[f]) - 0.5))
    return features

# linear feature extractor with interaction terms
def featureExtractorInteraction(state, action):
    port, binIdx, stockState = state

    featuresKey = ['binReturnNorm','return3Norm','return12Norm','returnSODNorm','prevDayReturnNorm',
                    'binReturnNorm.xlk','return3Norm.xlk','return12Norm.xlk','returnSODNorm.xlk','prevDayReturnNorm.xlk',
                    'binReturnNorm.spy','return3Norm.spy','return12Norm.spy','returnSODNorm.spy','prevDayReturnNorm.spy']

    features = featureExtractorSimple(state, action)
    if False: 
        features.append(((action, 'binReturnNorm*binReturnNorm.spy'), sigmoid(stockState['binReturnNorm'] * stockState['binReturnNorm.spy']) - 0.5))
        features.append(((action, 'return3Norm*return3Norm.spy'), sigmoid(stockState['return3Norm'] * stockState['return3Norm.spy']) - 0.5))
        features.append(((action, 'return12Norm*return12Norm.spy'), sigmoid(stockState['return12Norm'] * stockState['return12Norm.spy']) - 0.5))
        features.append(((action, 'returnSODNorm*returnSODNorm.spy'), sigmoid(stockState['returnSODNorm'] * stockState['returnSODNorm.spy']) - 0.5))
    features.append(((action, 'binReturnNorm*binReturnNorm.xlk'), sigmoid(stockState['binReturnNorm'] * stockState['binReturnNorm.xlk']) - 0.5))
    features.append(((action, 'return3Norm*return3Norm.xlk'), sigmoid(stockState['return3Norm'] * stockState['return3Norm.xlk']) - 0.5))
    features.append(((action, 'return12Norm*return12Norm.xlk'), sigmoid(stockState['return12Norm'] * stockState['return12Norm.xlk']) - 0.5))
    features.append(((action, 'returnSODNorm*returnSODNorm.xlk'), sigmoid(stockState['returnSODNorm'] * stockState['returnSODNorm.xlk']) - 0.5))

    features.append(((action, 'binReturnNorm', 'binVolState'), sigmoid(stockState['binReturnNorm']) - 0.5))
    features.append(((action, 'return3Norm', 'binVol3State'), sigmoid(stockState['return3Norm']) - 0.5))
    features.append(((action, 'return12Norm', 'binVol12State'), sigmoid(stockState['return12Norm']) - 0.5))
    features.append(((action, 'returnSODNorm', 'binVolSODState'), sigmoid(stockState['returnSODNorm']) - 0.5))
    features.append(((action, 'binReturnNorm', 'binState'), sigmoid(stockState['binReturnNorm']) - 0.5))
    features.append(((action, 'return3Norm', 'binState'), sigmoid(stockState['return3Norm']) - 0.5))
    features.append(((action, 'return12Norm', 'binState'), sigmoid(stockState['return12Norm']) - 0.5))
    features.append(((action, 'returnSODNorm', 'binState'), sigmoid(stockState['returnSODNorm']) - 0.5))
    
    return features

####################################################################################################
#                                       Q Learner                                              
####################################################################################################

class RLAlgorithm:
    def getAction(self, state): raise NotImplementedError("Override me")

    def incorporateFeedback(self, state, action, reward, newState): raise NotImplementedError("Override me")

# policy: opt or maxdiff 
class QLearningAlgorithm(RLAlgorithm):
    def __init__(self, actions, discount, featureExtractor, explorationProb=0.2, minStepSize=0.01, policy='epsGreedy'):
        self.actions = actions
        self.discount = discount
        self.featureExtractor = featureExtractor
        self.explorationProb = explorationProb
        self.weights = collections.defaultdict(float)
        self.numIters = 0
        self.policy = 'softmax' if policy == 'softmax' else 'epsGreedy'
        self.verbose = False
        self.minStepSize = minStepSize

    def setVerbose(self, vrbose):
        self.verbose = vrbose

    def setExplorationProb(self, prob):
        self.explorationProb = prob

    # Return the Q function associated with the weights and features
    def getQ(self, state, action):
        score = 0
        for f, v in self.featureExtractor(state, action):
            score += self.weights[f] * v
        return score

    def getAction1(self, state):
        self.numIters += 1
        if random.random() < self.explorationProb:
            return random.choice(self.actions(state))
        else:
            if self.policy == 'epsGreedy':
                return max((self.getQ(state, action), action) for action in self.actions(state))[1]
            else:
                return max((self.getQ(state, action), action) for action in self.actions(state))[1]

    def getAction(self, state):
        self.numIters += 1
        if self.policy == 'epsGreedy':
            if random.random() < self.explorationProb:
                return random.choice(self.actions(state))
            else:
                return max((self.getQ(state, action), action) for action in self.actions(state))[1]
        else:
            cumprobs = np.cumsum(softmax([self.getQ(state, action) for action in self.actions(state)]))
            return self.actions(state)[np.argmax(random.random() < cumprobs)]

    # Call this function to get the step size to update the weights.
    def getStepSize(self):
        return max(1.0 / math.sqrt(self.numIters), self.minStepSize)

    def incorporateFeedback(self, state, action, reward, newState):
        if newState == None:
            return
        Vopt = -float('inf')
        for nextAction in self.actions(newState):
            Vopt = max(Vopt, self.getQ(newState, nextAction))
        delta = self.getQ(state, action) - (reward + self.discount * Vopt)
        for f, v in self.featureExtractor(state, action):
            self.weights[f] = self.weights[f] - self.getStepSize() * delta * v 
        if self.verbose:
            print(self.numIters, ': weights', self.weights)


####################################################################################################
#                                           Simulator                                              
####################################################################################################
# routine to simulate RL under a given MDP 
def simulate(mdp, rl, numTrials=10, maxIterations=1000, verbose=False, saveBehavior=False):
    def sample(probs):
        target = random.random()
        accum = 0
        for i, prob in enumerate(probs):
            accum += prob
            if accum >= target: return i
        raise Exception("Invalid probs: %s" % probs)

    totalRewards = []  # The rewards we get on each trial
    idate = ibin = iaction = ireward = []
    for trial in range(numTrials):
        state = mdp.startState()
        #sequence = [state]
        sequence = []
        totalDiscount = 1
        totalReward = 0
        for _ in range(maxIterations):
            action = rl.getAction(state)
            transitions = mdp.succAndProbReward(state, action)
            if len(transitions) == 0:
                rl.incorporateFeedback(state, action, 0, None)
                break

            # Choose a random transition
            i = sample([prob for newState, prob, reward in transitions])
            newState, prob, reward = transitions[i]
            #sequence.append(action)
            #sequence.append(reward)
            #sequence.append(newState)
            sequence.append((state[1], action, reward))

            rl.incorporateFeedback(state, action, reward, newState)
            totalReward += totalDiscount * reward
            totalDiscount *= mdp.discount()
            state = newState

            if saveBehavior:
                idate = idate + [mdp.simCurrentDate]
                ibin  = ibin + [state[2]['bin']]
                iaction = iaction + [action]
                ireward = ireward + [reward]
        if verbose:
            print("Trial %d (totalReward = %s): %s" % (trial, totalReward, sequence))
        totalRewards.append(totalReward)
        if not mdp.canSimulate():
            break
        mdp.nextSimTrial()
    return (totalRewards, pd.DataFrame({'date':idate, 'bin':ibin, 'action':iaction, 'reward':ireward}))

def sigmoid(x):
    sgn = 1 if x > 0 else -1
    return 1 / (1 + math.exp(sgn * min(abs(x), 20)))

def sigmoid(x):
    sgn = 1 if x > 0 else -1
    return 1 / (1 + math.exp(sgn * min(abs(x), 20)))

def softmax(x):
    x = x - np.max(x, axis=0)
    exp = np.exp(x)
    s = exp / np.sum(exp, axis=0)
    return s
