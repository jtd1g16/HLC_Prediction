# HLC_Predictor
*Computational Chemistry Masters Project - University of Southampton*
A set of Jupyter notebooks illustrating a Henry's law constant (HLC) predictive model, starting from a species' SMILES string.

The compilation of HLCs used in this project was created by R. Sander, the paper published is [available here](https://www.semanticscholar.org/paper/Compilation-of-Henry's-law-constants-(version-4.0)-Sander/c61dc4a148ae9f5913ab5c8db96d120269701eb1).

The CAS reference numbers in the compilation were used to create SMILES strings (via [cirpy](https://cirpy.readthedocs.io/en/latest/)). These were in turn passed through [DRAGON](http://www.talete.mi.it/index.htm) or a series of [RDkit](https://www.rdkit.org/) functions to calculate molecular descriptors.

Supervised machine learning algorithms were trained (using the calculated descriptors labelled with their molecules' HLCs) to predict the constants.

- 7 ML algorithms
- 4 feature selection methods
- 6 sets of descriptors

# Dissertation coming soon to an eAssignments near you!

## Dependancies
- Jupyter notebooks, with the following python packages installed:
	- `pandas` (data structures)
	- `numpy` (maths)
	- `statsmodels.api` (stats)
	- `cirpy` (conversion between chemical identifiers)
	- `ipywidgets` and `IPython.display` (widgets and nicer outputs)
	- `RDKit` (descriptors)
	- `matplotlib.pyplot` (visualisation)
	- `scikit-learn` (models, feature selection, PCA)
	- `joblib` (saving python objects)
	- `mpld3` (hover-over labels for plots)
- DRAGON 6 (not within python, external software for descriptor calculation)
