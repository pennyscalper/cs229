save code in folder code
save data in folder data
both the folders, code and data should be in the same directory
cd data
here we have
.R files to produce the features from raw data in data
run 
$./genFeatures.R -s 'AAPL'

the above will save bunch of files in /output folder
and also the L1-regression model and PCA outputs

.py files run the RL routine and LSTM, saves output in /output folder

run
$python stockTraderCS229.py AAPL
