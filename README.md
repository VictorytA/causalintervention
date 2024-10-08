# causalintervention

This repository is a supplement to the causal intervention paper. The code provided is specifically designed for the data used for the paper but the methods can be used for other datasets. 

## Data 

The data used in the paper comes from the Innowell platform. This data requires ethics approval, so it is only available upon request. 

## Code
Each code will have a comment provided at the top of each script, summarising the purpose and function of the script but the main steps used in the paper are:
1. Run the sampling scheme using 'Chains Processing.R' script.
2. Calculate edge probabilities and plot using 'DAG.R' script.
3. Calculate the domain's utility function using 'gRain_intervention.R' script.
4. Find the idealised interventions based on their expected utility with 'Intervention Summary.R' script.

## Libraries
Note that some of these codes use functions from internal libraries; 'cia' and 'rdigidata' which can be accessed from the following link: https://github.com/SpaceOdyssey/cia. The library 'rdigidata' is used to deal with Innowell data which is available upon request.

Some of the code also use libraries: tidyverse, gRain, doParallel, foreach, dagitty, ggplot2. Be sure to install these packages for the code to be reproducible 