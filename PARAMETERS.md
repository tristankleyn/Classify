| File | Parameter name | Description | Input type/range |
|---|---|---|---|
| Classify-ROCCA, Classify-delphinID | AndOr | whether to use minClicks 'AND' minWhistles thresholds or minClicks 'OR' minWhistles threshold | TRUE or FALSE |
| Classify-delphinID | cTable | name of table in PAMGuard database containing delphinID click classifications | text |
| Classify-delphinID | dateRange | range of dates for compiling acoustic events from PAMGuard database | vector of two dates (e.g. c(Sys.Date() - 1000, Sys.Date()) |
| Classify-delphinID | evScore | minimum decision score threshold below which event classifications are discarded. | numeric (0-1) |
| Classify-ROCCA, Classify-delphinID | export | binary setting for saving or not saving files | TRUE or FALSE |
| Classify-ROCCA | filterVars_max | list for specifying minimum limits for select variables | list |
| Classify-ROCCA | filterVars_min | list for specifying minimum limits for select variables | list |
| Classify-ROCCA | from_folders | binary setting for whether or not to load data from folder structure | TRUE or FALSE |
| Classify-ROCCA, Classify-delphinID | groupVar | name of grouping variable for classifier training and testing | text (name of variable) |
| Classify-delphinID | imputeTrain | minimum number of classifications below which to replace predictor variable with mean values from other points of same targetVar but not the same groupVar | numeric (1+) |
| Classify-ROCCA | levels | levels of folder structure for loading data | text vector e.g. c('species', 'location') |
| Classify-delphinID | minClicks | minimum number of click classifications below which event classifications are discarded. | numeric (0+) |
| Classify-ROCCA | minGroupScore | minimum decision score threshold for event classifications | numeric (0-1) |
| Classify-ROCCA | minScore | minimum decision score threshold for individual click/whistle classifications | numeric (0-1) |
| Classify-delphinID | minWhistles | minimum number of whistle classifications below which event classifications are discarded. | numeric (0+) |
| Classify-ROCCA, Classify-delphinID | mTry | the number of predictor variables used per decision tree node | numeric (1+) |
| Classify-ROCCA | nMax | the maximum number of observations to sample from each group for training classifiers | numeric (1+) |
| Classify-ROCCA, Classify-delphinID | nodeSize |   | numeric (1+) |
| Classify-ROCCA, Classify-delphinID | nTrees | number of decision trees to use in a Random Forest model | numeric (1+) |
| Classify-ROCCA | omitGroups | vector of groups to omit from classifier training and testing | text vector e.g. c('group1', 'group2') |
| Classify-ROCCA | omitVars | vector of variables to omit from classifier training and testing | text vector e.g. c('FREQMAX', 'DURATION') |
| Classify-ROCCA, Classify-delphinID | plot_DPI | DPI (dots per inch) setting for saving figures | numeric (1+) |
| Classify-ROCCA, Classify-delphinID | plot_dims | dimensions in centimetres for saving figures | numeric (1+) |
| Classify-ROCCA | point_size | point size for scatterplots | numeric (1+) |
| Classify-ROCCA, Classify-delphinID | point_transparency | transparency of scatterplot points | numeric (0-1) |
| Classify-delphinID | plotType | type of plot outputted for visualizing event classifications | text ("Map" or "Counts") |
| Classify-ROCCA, Classify-delphinID | pruneTrain | percentage of data points furthest from their target variable centroid to discard from the training data | numeric (0-1) |
| Classify-ROCCA | rootDirectory | base directory containing .csv data exported from PAMGuard | text |
| Classify-ROCCA | savefolder | folder for exporting to (leave as NULL to automatically name folder by current date and time | text (path name) |
| Classify-delphinID | selectDB | path to PAMGuard database (from folder containing Classify-delphinID.qmd | text |
| Classify-ROCCA | selectGroups | vector of groups to limit classifier training and testing to | text vector e.g. c('group1', 'group2') |
| Classify-ROCCA, Classify-delphinID | targetVar | name of target variable for classifier training and testing | text (name of variable) |
| Classify-ROCCA | VARIABLE1 | x-axis variable for plotting (Required) | text (name of variable) |
| Classify-ROCCA | VARIABLE2 | y-axis variable for plotting (Optional) | text (name of variable) |
| Classify-ROCCA, Classify-delphinID | verbose | whether or not to print information during loading | TRUE or FALSE |
| Classify-ROCCA | vocType | type of vocalization for analysis | 'whistle', 'click', or 'other' |
| Classify-delphinID | wTable | name of table in PAMGuard database containing delphinID whistle classifications | text |
