# refgame_stimuli_methods
Materials accompanying the paper "High task performance may not be the result of successful reasoning: On the importance of eliciting participants' reasoning".

The data/ folder contains anonymized data for the 4 experiments described in the paper. 
- all_experiments.csv contains all reference game data
- all_annotations.csv contains strategy annotations

Statistical analysis scripts written in R can be found in the scripts/ folder.
- statistical_analysis.R contains Bayesian regression models for the 4 experiments, whose results are summarized in Table 1 in the paper.
- annotations_plot.R generates barplots of annotations (Figures 3 and 8 in the paper).
- lpa.R contains Latent Profile Analysis models for Experiments 1 and 4 and generates Figure 9 in the paper.
- stat_helpers.R is a file with helper functions (for outlier removal etc.) used by the other scripts.
