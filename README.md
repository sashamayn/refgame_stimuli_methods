# refgame_stimuli_methods
Materials accompanying the paper "High task performance may not be the result of successful reasoning: On the importance of eliciting participants' reasoning".

The data/ folder contains anonymized data for the 4 experiments described in the paper. 
- all_experiments.csv contains all reference game data
- all_annotations.csv contains strategy annotations

Statistical analysis scripts written in R can be found in the scripts/ folder.
- statistical_analysis.R contains Bayesian regression models for the 4 experiments, whose results are summarized in Table 1 in the paper.
- annotations_plot.R generates barplots of annotations (Figures 3 and 9 in the paper).
- lpa.R contains Latent Profile Analysis models for Experiments 1 and 4 and generates Figures 7 and 10 in the paper.
- stat_helpers.R is a file with helper functions (for outlier removal etc.) used by the other scripts.

The folder scripts/RData/ contains preprocessed data (dfs_for_logregr.RData) that can be used to run the brms models to obtain the numbers reported in the paper in Table 1. If you run the statistical_analysis.R script anew, the numbers may differ slightly because of the randomization of the ambiguous condition (see the paper for more details). It also contains barplots with average performance in each of the conditions for each experiment (barplots.RData), similar to left panel of Fig 2. in Franke & Degen (2016). These plots are not included in the paper but we believe that they are an intuitive visualization of the effects of condition across experiments reported in Table 1 of the paper.
