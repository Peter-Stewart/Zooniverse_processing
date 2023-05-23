# Zooniverse_processing (PhD Thesis Code)

This repository contains code for chapters 4 and 5 of my PhD thesis.

Code for chapter 3, "Model selection in occupancy models: inference vs. predictions" can be found at: [Peter-Stewart/Model_selection_in_occupancy_models: v2.0.1 | Zenodo](https://doi.org/10.5281/zenodo.7043335)

Files are shown in **bold**, directories are shown in *italics*.

- **Zooniverse_processing.R** - main script for processing the Zooniverse classification data, creating output files which are subsequently used in the other scripts
  
- **helper_functions.R** - custom functions used in the other R scripts
  
- *ch4* - R scripts for chapter 4, "Impacts of invasive *Opuntia* cacti on mammalian habitat use"
  
  - **accuracy_validation.R** - analysis of volunteer classification accuracy (for Appendix D)
    
  - **ch4_analysis.R** - main analysis for chapter 4, containing all occupancy and activity analyses for main text and Appendix E
    
  - **distance_sampling.R** - distance sampling for estimating grid-square-level *Opuntia* density, which is then used in main analysis
    
  - **prior_predictive_simulations.R** - prior predictive simulations for Appendix B
    
  - **simulation_validation.R** - validation of Gaussian process occupancy model on synthetic data for Appendix C
    
- *ch5* - R scripts for chapter 5, "Interactions between invasive *Opuntia* cacti and native animals"
  
  - **ch5_analysis.R** - all R code for chapter 5 including analyses for main text and Appendix C, and prior predictive simulations for Appendix B
    
- *models* - Stan models used in chapters 4 and 5
  
  - *ch4* - models for chapter 4
    
    - **hurdle_total_novegpath.stan** - hurdle model for total effect of *Opuntia* on activity, assuming no vegetation pathway
      
    - **hurdle_total_vegpath.stan** - hurdle model for total effect of *Opuntia* on activity, assuming vegetation pathway
      
    - **mbj_distance.stan** - distance sampling model
      
    - **sim_test.stan** - simplified Gaussian process occupancy model for validation on synthetic data
      
    - **total_effect_no_veg_path.stan** - Gaussian process occupancy model for total effect of *Opuntia* on occupancy, assuming no vegetation pathway
      
    - **total_effect_veg_path.stan** - Gaussian process occupancy model for total effect of *Opuntia* on occupancy, assuming vegetation pathway
      
  - *ch5* - models for chapter 5
    
    - **binomial.stan** - binomial model for fruit presence/absence
      
    - **binomial_impute.stan** - binomial model for fruit presence/absence, assuming latent site-level confound
      
    - **neg_bin.stan** - negative binomial model for fruit number
      
    - **neg_bin_impute.stan** - negative binomial model for fruit number, assuming latent site-level confound
      
- *helper_functions* - individual files containing the functions from **helper_functions.R** (NB: these individual files are not used in the current versions of the other scripts, but are being retained for future use)

