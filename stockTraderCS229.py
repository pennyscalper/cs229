#!/usr/bin/python
from qLearning import *
from lstm import *

def main():
    for arg in sys.argv[1:]:
        print arg
    if(len(sys.argv) < 2):
        print 'program requires ticker name...quitting'
        return(0)

    ticker = str(sys.argv[1]) 
    dataDir = '../data' 
    outputDir = '../outputQLearning' 
    lstmOutdir = '../outputLSTM' 
    if not os.path.exists(dataDir):
        raise Exception(dataDir + ' does not exists')
    if not os.path.exists(outputDir):
        raise Exception(outputDir + ' does not exists')
    port = 100 
    transactCost = 0 * 1e-4
    eod = 390
    explorationProb = 0.2
    startYear = 2014
    endYear = 2018
    saveTradingBehavior=True
    verbose=False
    policy='epsGreedy'

    print('QLearning...feature extrator with interaction terms to trade stock and sector')
    mdp = PortReturnPathMDP(ticker=ticker, dataDir=dataDir, port=port, transactCost=transactCost, eod=eod, randomize=False, startYear=startYear, endYear=endYear)
    fextractor = featureExtractorInteraction 
    for policy in ['epsGreedy', 'softmax']:
        eps = [0.05, 0.1, 0.2]
        if(policy == 'softmax'): eps = [0.05]
        for ep in eps:
            for ss in [0.0005, 0.005, 0.001]:
                mdp.reset()
                rl = QLearningAlgorithm(mdp.actions, mdp.discount(), fextractor,policy=policy, explorationProb=ep, minStepSize=ss)
                rewards = simulate(mdp, rl, numTrials=2000, verbose=verbose, saveBehavior=saveTradingBehavior)
                if saveTradingBehavior:
                    outfile = outputDir + '/ql.' + ticker + '.interactionFeatureArbTrading_' + str(policy) + '_' + str(ep) + '_' + str(ss) +'_' + '.csv'
                    rewards[1].to_csv(outfile)


    print('running LSTM')
    data = pd.read_csv(dataDir + '/' + 'features_5Mins_' + ticker + '.csv')
    features = ['binSqrt','binVolNorm','binReturnNorm','binReturnNorm.xlk','binReturnNorm.spy','return12Norm','return12Norm.xlk','return12Norm.spy','binVol12State','binVolState']

    nettype = 'lstm'
    trainYear = 2016
    nfeatures = len(features) - int('binVolState' in features) - ('binVol12State' in features)
    dummy = (3 if 'binVolState' in features else 1) + (3 if 'binVol12State' in features else 1)
    c1 = 1e-4
    c2 = 4.5e-7
    cvdf = pd.DataFrame(columns=['ifsigmoid', 'layers', 'nodes', 'lag', 'L1penalty', 'valLoss', 'trainLoss', 'testLoss'])
    for ifsigmoid in [True, False]:
        for layers in range(1,3):
            for nodes in range(3, 6):
                lagrange = range(2, 6)
                if nettype == 'dense':
                    lagrange = [0]
                for lag in lagrange:
                    nwts = nodes * (nfeatures + dummy) * (lag+1) * layers + nodes * 2
                    for L1penalty in np.flip(np.exp(math.log(c2) + math.log(c1/c2)/20.0 * np.arange(21)) * math.sqrt(nwts / 258)):
                        train_x, train_y, test_x, test_y = lstmPrepareData(data, features, lag, trainYear, ifsigmoid)
                        if nettype == 'dense':
                            train_x = train_x[:, 0, :]
                            test_x = test_x[:, 0, :]
                            model = lstmBuildModel(train_x.shape[1], 0, nodes, layers, L1penalty, optname='adagrad', nettype=nettype)
                        else:
                            model = lstmBuildModel(train_x.shape[2], train_x.shape[1], nodes, layers, L1penalty, optname='adagrad', nettype=nettype)
                        out = model.fit(train_x, train_y, epochs=20, batch_size=36, validation_split=0.1)
                        predict = model.predict(test_x)
                        testLoss = model.evaluate(test_x, test_y)
                        cvdf = pd.concat([cvdf, pd.DataFrame({'ifsigmoid':ifsigmoid, 'layers':[layers], 'nodes':nodes, 'lag':lag, 'L1penalty':L1penalty, 'valLoss':out.history['val_loss'][-1], 'trainLoss':out.history['loss'][-1], 'testLoss':testLoss})], axis=0)
                        preddf = data[data.year > trainYear][['year','date','bin','time']]
                        preddf['predict'] = predict[:,0]
                        preddf['ifsigmoid'] = ifsigmoid
                        preddf['layers']=layers
                        preddf['nodes']=nodes
                        preddf['lag']=lag
                        preddf['L1penalty']=L1penalty
                        preddf.to_csv(lstmOutdir + '/' + ticker + '_' + str(ifsigmoid) + str(layers) + '_' + str(nodes) + '_' + str(lag) + '_' + str(L1penalty) + '.csv', sep='\t')
                
    cvdf.to_csv(lstmOutdir + '/' + ticker + '_' + nettype + '_' + str(layers) + '_' + 'CVDF' + '.csv', sep='\t')

if __name__ == "__main__":
    main()
