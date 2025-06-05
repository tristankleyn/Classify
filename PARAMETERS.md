| Parameter name | Description | Input type/range |
|---|---|---|
| alpha | transparency of scatterplot points | numeric (0-1) |
| export | binary setting for saving or not saving files | TRUE or FALSE |
| plot_DPI | DPI (dots per inch) setting for saving figures | numeric (1+) |
| plot_dims | dimensions in centimetres for saving figures | numeric (1+) |
| filterVars_max | list for specifying minimum limits for select variables | list |
| filterVars_min | list for specifying minimum limits for select variables | list |
| from_folders | binary setting for whether or not to load data from folder structure | TRUE or FALSE |
| groupVar | name of grouping variable for classifier training and testing | text (name of variable) |
| levels | levels of folder structure for loading data | text vector e.g. c('species', 'location') |
| minScore | minimum decision score threshold for individual click/whistle predictions | numeric (0-1) |
| mTry | the number of predictor variables used per decision tree node | numeric (1+) |
| nMax | the maximum number of observations to sample from each group for training classifiers | numeric (1+) |
| nodeSize |  | numeric (1+) |
| nTrees | number of decision trees to use in a Random Forest model | numeric (1+) |
| omitGroups | vector of groups to omit from classifier training and testing | text vector e.g. c('group1', 'group2') |
| omitVars | vector of variables to omit from classifier training and testing | text vector e.g. c('FREQMAX', 'DURATION') |
| pruneTrain | percentage of data points furthest from their target variable centroid to discard from the training data | numeric (0-1) |
| rootDirectory | base directory containing .csv data exported from PAMGuard | text |
| resultsFolder |  | text (path name) |
| selectGroups | vector of groups to limit classifier training and testing to | text vector e.g. c('group1', 'group2') |
| size | point size for scatterplots | numeric (1+) |
| targetVar | name of target variable for classifier training and testing | text (name of variable) |
| VARIABLE1 | x-axis variable for plotting (Required) | text (name of variable) |
| VARIABLE2 | y-axis variable for plotting (Optional) | text (name of variable) |
| vocType | type of vocalization for analysis | 'whistle', 'click', or 'other' |
