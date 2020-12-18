In this project I work out code and then perform a power analysis for the proposed Elliott State Research Forest design.  This involves the simplest version of a power analysis, focused on the four proposed management treatments at the subwatershed-level, not the stand level.

I work out the process with different means and variances per management treatment based on a Welch ANOVA in 2020-08_basic_power_analysis.R.  This assumes response variables will be reasonably normally distributed.

I created an interactive dashboard based on the code in the file above so folks can input their own numbers and explore how things change.  The code is in the 2020-08_basic_power_analysis_dashboard.Rmd file.  I published this app at https://aosmith.shinyapps.io/2020-08_basic_power_analysis_dashboard.

Finally, I did the actual power analysis for a variety of potential response variables in 2020-11_elliott_power_analysis.R.  Outputs from there (plots and other information) were included in the Elliott State Research Forest proposal submitted to Oregon's State Land Board in December 2020.

See more about the Elliott State Research Forest proposal [here](https://www.forestry.oregonstate.edu/elliott-state-forest).