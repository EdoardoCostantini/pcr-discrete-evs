# PCR on EVS data

Studying different ways of coding categorical data for PCR.
Resampling study to compare the out-of-sample prediction performance of Principal Component Regression with discrete data as input data.

## Summary of project

The goal of the study was to understand how different ways of coding ordinal data impact the quality of the low-dimensional representation obtained by PCA for prediction tasks.
The quality of the representation is assessed based on the prediction error obtained by using the PCs extracted based on the different coding schemes.
The evaluation is performed using [EVS](https://europeanvaluesstudy.eu) data as input.

### Resampling study procedure

The resampling study procedure involved:

1. Draw a **bootstrap sample** of the EVS data (i.e., sample same a sample of the same size with replacement)
2. **Computation of the PCs** for the prediction of a target variable $y$ (a set of variables $X$ is prepared and the PCs are computed as their linear combinations)
3. Compute the **out-of-sample prediction error** (see below for details):

  - means squared error (MSE) for the prediction of numerical/ordinal variables;
  - [cross-entropy](https://rpubs.com/juanhklopper/cross_entropy) (centropy) for the prediction of the binary and multi-categorical variables;
- accuracy of the prediction class computed as the proportion of correctly classified cases for the prediction of the binary and multi-categorical variables.

### Coding schemes compared

In step 2, the PCs are computed based on different processing of the data:

- **Mixed data treatment**: ordinal, and count data are treated as continuous, while binary and categorical data data treated as categorical data.
  - **Dummy**: the PCs are extracted from a design matrix combining the data treated as continuous with dummy codes of the discrete variables.
  - **Disjunctive table**: the PCs are extracted from a design matrix combining the data treated as continuous with the complete disjunctive table of the discrete variables.
  - **PCAmix**: the PCs are extracted from a design matrix combining the data treated as continuous with the disjunctive table of the categorical data, but scaling is performed as described by [Chavent et. al. (2017)](https://arxiv.org/abs/1411.4911).
- **Categorical treatment**: all variables are treated as categorical.
  - **Dummy**: the PCs are extracted from the design matrix with dummy codes of the discrete variables.
  - **Disjunctive table**: the PC are extracted from the complete [disjunctive table](https://www.xlstat.com/en/solutions/features/complete-disjuncive-tables-creating-dummy-variables) of the discrete variables. The disjunctive table creates a dummy variable *for each* category of the discrete variable. This method is also known as *Filmer–Pritchett*.
  - **MCA/PCAmix**: the PCs are extracted by applying standard Correspondence Analysis (CA) to the disjunctive table of the data (PCAmix applied to data treated as categorical is MCA).

### Variables to be predicted


### Resampling study fixed factors

These parameters were kept constant in the simulation study:

- test sample size (0.1 of the total sample size)

### Resampling study experimental factors

The simulation study procedure is repeated for each of the conditions resulting by the crossing of the following experimental factors:

- the **dependent variable** to be predicted:

  I wanted to assess the predictive performance for three types of variables:

  - **numerical**: Any 10 points 'feeling thermometer' is usually treated as continuous by researchers working with questionnaires. I chose to predict two variables in this group:
    - 'political party: appeals to you most: left/right scale (Q49)' (v174_LR) as it is a 10-points item that is reasonably normally distributed.
    - 'how satisfied are you with your life (Q10)' (v39) a 10-points item with reasonably symmetric distribution
    
  - **binary**: Any variable recording a yes / no opinion would work. Social 'trust' is often a construct of interest to researchers using EVS data. Therefore, I chose to predict the response the agreement with the statement that most people can be trusted (v31). An example paper trying to predict this variable can be found [here](https://doi.org/10.1177/1948550621999272).

  - **categorical**: Any variable recording membership of the respondents to a group could be used.  I decided to use
    - 'what group your household is' (v261) the income group to which individuals belong as a dependent variable for this predictive task (the item name in the EVS data set is 'households total net income (Q98) (standardized)', the variable name is 'v261').
    - 'Which of these statements' comes closest to your beliefs?' (v62): A question with four possible responses describing the person's beliefs around God and spirituality.

  This variable could have been used as an ordinal item, but its close-to-uniform distribution in the EVS data makes the categorical treatment preferable.

- the **number of principal components** (npcs) to be computed:
  - two fixed values: 1 and 10 PCs
  - two non-graphical decision rules (acceleration factor and kaiser rule)
  - nine proportion of explained variance (`seq(0.1, 0.9, .1)`)


## Repository structure

Here is the project structure:

```
pcr_discrete_evs
├── code
│ ├── functions
│ │ ├── dropVars.R
│ │ ├── extractMSE.R
│ │ ├── extractPCAmix.R
│ │ ├── extractPCs.R
│ │ ├── generateBS.R
│ │ ├── generateDV.R
│ │ └── orthmat.R
│ ├── helper
│ │ ├── readTarGz.R
│ │ └── writeTarGz.R
│ ├── scratch
│ │ └── niekdata.R
│ ├── subroutines
│ │ ├── doRep.R
│ │ └── runCell.R
│ ├── fs.R
│ ├── init.R
│ ├── run_sim.R
│ ├── script_analysis.R
│ ├── script_pooling.R
│ └── script_prepEVS.R
├── docs
│ ├── ZA7500_bq_CAPI.pdf
│ ├── ZA7500_bq_CAWI.pdf
│ ├── ZA7500_cdb.pdf
│ └── ZA7500_cdb_App_B_Income.pdf
├── graphs
├── input
│ ├── ZA7500_processed.rds
│ ├── ZA7500_v4-0-0.dta
│ ├── ZA7500_v4-0-0_missing.txt
│ └── var_types.rds
├── output
│ └── ZA7500_mi.rds
└── README.md

```

Here is a brief description of the folders:

- code: the main software to run the study
  - `functions` folder with the main project specific functions
  - `helper` folder with functions to address file management and other small internal tasks
  - `subroutines` folder with the generic functions to run the simulation study
  - main R scripts for the project
- graphs: folder to store plots
- input: folder where all the input files (e.g., data) should be stored
- output: folder where the results of scripts located in code are stored
