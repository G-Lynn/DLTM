# DLTM
This repository contains all R and C++ software developed for inference and post-processing in the Dynamic Linear Topic Model.

There are a few files which need to be run in order to run the MCMC simulations and post-processing.  

The MCMC algorithm can be run in main.R.  The user must update the directory of the DLTM-master folder. This directory variable, called stem, must be a full path.  It may not contain any abbreviations
such as ~/, .., or other linux based directory abbreviations.  The reason is that output files are written in C++ and C++ does not recognize these abbreviations.  A few other user defined inputs must be changed at the very top of main.R.  The user should not have to change anything after the point that says "End of user defined inputs"

All post-processing code is in the folder Post_Processing.  A few directories and user defined inputs may require updating at the top of each file.
  
In order to run the post-processing steps for the case where the MCMC is being benchmarked against a known truth, run the file Post_Processing/Benchmark.R.
To compare the MCMC output when the number of topics is misspecified at K=6, see the file Post_Processing/Benchmark_K6.R.
To compare the the output from multiple MCMC runs, execute the file Post_Processing/reproducibility_comparison.R.  
To compute the within chain and across chain variability of the MCMC outputs, run the file Post_Processing/Gelman_Rubin_TV.R
 
If the MCMC is for a real data set, run the file Post_Processing/PubMed_Post_Processing.R.  A few user defined inputs require changing at the beginning.
If the user wishes to generate Google Motion charts to explore the topics, run the file Post_Processing/Motion_Charts.R.  Again, directories will require updating.  





