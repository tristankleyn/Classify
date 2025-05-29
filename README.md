## ClassifyStuff: train and test acoustic classifier models
🐬  🔊  🦜  🔊  🐒  🔊  🐦  🔊  🦇  🔊  🐳  🔊  🐠  🔊  🐞  🔊  🦅  🔊  🐘  🔊  🐬

<br> 

This repository contains a Quarto notebook designed to facilitate training and testing of machine learning acoustic classifier models to classify acoustic data to preset labels. Currently designed specifically for dealing with data exported from [PAMGuard](https://www.pamguard.org/).

<br>
  
### ClassifyStuff/

> #### └── [Classify.qmd](https://github.com/tristankleyn/ClassifyStuff/blob/master/Classify.qmd)
> User-friendly Quarto notebook for developing acoustic classifiers.

> #### └── [Classify_functions.R](https://github.com/tristankleyn/ClassifyStuff/blob/master/Classify_functions.R)
> Required functions for Classify.qmd

> #### └── [PARAMETERS.md](https://github.com/tristankleyn/ClassifyStuff/blob/master/PARAMETERS.md)
> Appendix of adjustable parameters in Classify.qmd


<br>

### Requirements
- R
- RStudio, VS Code, or other IDE
  
### DelphinID North East Atlantic
delphinID classifier for the North East Atlantic - processes detected clicks and whistle contours to determine 7 delphinid species

delphinID models are deep convolutional neural networks (CNNs) trained in Python Tensorflow 2.18.0 to automatically classify detections of delphinid whistles and clicks to species based on the distributions of observed frequencies in groups of whistles or clicks detected using PAMGuard's Whistle & Moan Detector and Click Detector, respectively. delphinID click and whistle models for the Northeast Atlantic were trained using groups of detections within 4-second time windows, while the output of these models is used as a feature vector for additional Random Forest layer which provides a recording-level species classification based on acoustic information from both whistles and clicks. As the detection-based input for classification is computationally inexpensive to compute, delphinID can easily run in PAMGuard at up speeds up to 64x real-time.

Cross-validated testing estimates delphinID to classify recordings with an average accuracy of 86%, which is expected to vary between species from 80% for Delphinus delphis to 92% for Lagenorhynchus albirostris. While increased detection counts within recordings were not found to significantly influence average classification accuracy, they did contribute to smaller variance in accuracy (more stability in classifier performance). Species that can be classified by the Northeast Atlantic delphinID models are:

- Atlantic white-sided dolphin (Lagenorhynchus acutus)
- Common bottlenose dolphin (Tursiops truncatus)
- Killer whale (Orcinus orca)
- Long-finned pilot whale (Globicephala melas)
- Risso’s dolphin (Grampus griseus)
- Short-beaked common dolphin (Delphinus delphis)
- White-beaked dolphin (Lagenorhynchus albirostris)

### Reference
Paper in prep.

### Models
Northeast Atlantic delphinID click and whistle classifiers can be found on Zenodo. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14578299.svg)](https://doi.org/10.5281/zenodo.14578299)

delphinID does not require raw sound data; instead it process detected PAMGuard whistle contours and click detections. Separate click and whistle predictions are intended to be combined for final species classification via an R Shiny App also accessible through the Zenodo download. Also included in the download are a PAMGuard settings file (.psfx) for delphinID and a README file in the download containing documentation, tutorials, and extra information.

As well as the usual setting the location of the database, binary store and sound files, users also need to open both deep learning modules e.g. Settings -> Raw deep learning classifier_clicks and select the deep learning models on their local machine. The configuration will then detect whistle contours and clicks from raw sound data, passing the detections to both deep learning modules with results saved in the PAMGuard database. Also note that delphinID can run in viewer mode if whisltes and/or click detections are part of the data model.


