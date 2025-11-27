# chatgpt_perception_ai
Scripts for the paper [Changes in public perception of artificial intelligence in healthcare after exposure to ChatGPT](https://doi.org/10.1038/s41746-025-02169-x).

`data_analyses.qmd` contains the code used to analyse the raw data on Statistics Denmark’s servers. This script produced the three output files found in the `analysis_outputs` folder, each containing estimates from one of three regression models. The forest plots in the paper are generated from these estimates with `plotting_script.R` (which depends on `load_packages.R`).
