#!/usr/bin/python

import sys, os, math, random
from collections import defaultdict
import pandas as pd
import numpy as np
from keras.models import Sequential
from keras.layers import Dense, Activation, LSTM
import keras
import keras.backend as K
from keras import regularizers
from matplotlib import pyplot
from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.preprocessing import OneHotEncoder
from sklearn.metrics import mean_squared_error
os.environ['KMP_DUPLICATE_LIB_OK']='True'

def concatHistory(xdata, n_in=1, n_out=1, dropnan=True):
    n_vars = 1 if type(xdata) is list else xdata.shape[1]
    df = pd.DataFrame(xdata)
    cols, names = list(), list()
    # input sequence (t-n, ... t-1)
    for i in range(n_in, 0, -1):
        cols.append(df.shift(i))
        names += [('var%d(t-%d)' % (j+1, i)) for j in range(n_vars)]
    # forecast sequence (t, t+1, ... t+n)
    for i in range(0, n_out):
        cols.append(df.shift(-i))
        if i == 0:
            names += [('var%d(t)' % (j+1)) for j in range(n_vars)]
        else:
            names += [('var%d(t+%d)' % (j+1, i)) for j in range(n_vars)]
    # put it all together
    agg = pd.concat(cols, axis=1)
    agg.columns = names
    # drop rows with NaN values
    if dropnan:
        agg.dropna(inplace=True)
    return agg

def h_vec(x):
    return 1 / (1 + np.exp(-x))

def lstmPrepareData(data, features, lag, trainYear, ifsigmoid):
    ydata = data['y']
    featuresToCollapse = [f for f in features if f not in ['binVolState', 'binVol12State']]
    xdata = data[featuresToCollapse]
    if ifsigmoid == True:
        xdata = (xdata - xdata.mean()) / xdata.std()
        #xdata = xdata.apply(h_vec)

    if 'binVolState' in features:
        onehotdt = pd.get_dummies(data.binVolState)
        onehotdt = onehotdt.drop(onehotdt.columns[1], axis=1)
        onehotdt.columns = 'binVolState' + onehotdt.columns
        xdata = pd.concat([xdata, onehotdt], axis=1) 
    if 'binVol12State' in features:
        onehotdt = pd.get_dummies(data.binVol12State) 
        onehotdt = onehotdt.drop(onehotdt.columns[1], axis=1)
        onehotdt.columns = 'binVol12State' + onehotdt.columns
        xdata = pd.concat([xdata, onehotdt], axis=1) 
    
    nx = xdata.shape[1]
    xdata = concatHistory(xdata, lag, 1)
    ydata = ydata.drop(ydata.index[range(lag)])
    train_x = xdata.loc[(data['year'] <= trainYear)]
    train_y = ydata.loc[(data['year'] <= trainYear)]
    test_x = xdata.loc[(data['year'] > trainYear)]
    test_y = ydata.loc[(data['year'] > trainYear)]
    
    ##reshape
    train_x = train_x.values.reshape((train_x.shape[0], lag+1, nx))
    train_y = train_y.values
    test_x = test_x.values.reshape((test_x.shape[0], lag+1, nx))
    test_y = test_y.values
    return train_x, train_y, test_x, test_y

def lstmBuildModel(nfeatures, lag, nodes, layers, L1penalty, optname='adam', nettype='lstm'):
    model = Sequential()
    if nettype == 'lstm':
        model.add(LSTM(nodes, input_shape=(lag, nfeatures), recurrent_regularizer=regularizers.l1(L1penalty), kernel_regularizer=regularizers.l1(L1penalty), bias_regularizer=regularizers.l1(L1penalty), recurrent_initializer='random_uniform',kernel_initializer='random_uniform', bias_initializer='zeros'))
    else:
        model.add(Dense(nodes, input_dim=nfeatures, kernel_regularizer=regularizers.l1(L1penalty), bias_regularizer=regularizers.l1(L1penalty), kernel_initializer='random_uniform', bias_initializer='zeros'))
    model.add(Activation('relu'))
    if layers > 1:
        model.add(Dense(nodes, input_dim=nodes, kernel_regularizer=regularizers.l1(L1penalty), bias_regularizer=regularizers.l1(L1penalty), kernel_initializer='random_uniform', bias_initializer='zeros'))
        model.add(Activation('relu'))

    model.add(Dense(1, input_dim=nodes, kernel_regularizer=regularizers.l1(L1penalty), bias_regularizer=regularizers.l1(L1penalty), kernel_initializer='random_uniform', bias_initializer='zeros'))
    if(optname == 'sgd'): opt = keras.optimizers.SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
    if(optname == 'adagrad'): opt = keras.optimizers.Adagrad(lr=0.1, epsilon=None, decay=0.0)
    if(optname == 'adam'): opt = ras.optimizers.Adam(lr=0.001, beta_1=0.9, beta_2=0.999, epsilon=None, decay=0.0, amsgrad=False)
    if(optname == 'rmsprop'): opt = keras.optimizers.RMSprop(lr=0.001, rho=0.9, epsilon=None, decay=0.0) 
    if(optname == 'adamax'): opt = keras.optimizers.Adamax(lr=0.002, beta_1=0.9, beta_2=0.999, epsilon=None, decay=0.0)
    if(optname == 'adadelta'): opt = keras.optimizers.Adadelta(lr=1.0, rho=0.95, epsilon=None, decay=0.0)    
    model.add(Activation("linear"))
    model.compile(loss='mean_squared_error', optimizer=opt)
    return model

    # fit network
    #history = model.fit(train_X, train_y, epochs=50, batch_size=72, validation_data=(test_X, test_y), verbose=2, shuffle=False)
