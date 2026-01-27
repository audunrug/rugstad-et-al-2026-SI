# Supporting Information to Rugstad et al. 2026
[![DOI](https://zenodo.org/badge/1030316476.svg)](https://doi.org/10.5281/zenodo.18001566)

This repository contains Supporting information, data and code to the manuscript "One One Toolbox, Many Tools: A Practicioner's Guide to Latent Variable Modeling for Common Ecological Questions" (Rugstad et al. 2026),


## Structure
The repository is organised as follows:
- `supplementary_1.qmd`: Quarto file for rendering Appendix S1
- `supplementary_2.qmd`: Quarto file for rendering Appendix S2
- `references.bib`: BibLaTex refernces for the main manuscript, S1 and S2
- `data_S1/`: Raw data used in Appendix S1
   - `boral_stats.txt`: Citations for the `boral` `R` package ([WOS query link](https://www.webofscience.com/wos/woscc/summary/e990d007-d9d2-43be-b9e7-e801367d04d8-019b102621/relevance/1))
   - `gllvm_stats.txt`: Citations for the `gllvm` `R` package ([WOS query link](https://www.webofscience.com/wos/woscc/summary/a4d11b86-19f3-4ab8-83ac-947f135339b9-019b100832/relevance/1))
   - `hmsc_stats.txt`: Citations for the `hmsc` `R` package ([WOS query link](https://www.webofscience.com/wos/woscc/summary/0547b4b6-7283-4c81-ac27-81008a603ef5-019b101887/relevance/1))
   - `vegan_stats.txt`: Citations for the `vegan` `R` package ([WOS query link](https://www.webofscience.com/wos/alldb/summary/a4b4285e-9adc-4393-ab30-595d0a3dd0d9-019b0fdd22/relevance/1))
- `figures/`: Figures included in the main manuscript
   - `figure_1.pdf`: Produced in Inkscape
   - `figure_2.pdf`: Produced in Inkscape
   - `figure_3.pdf`: Produced in Inkscape 
   - `figure_4.pdf`: Rendered by the script `example_1/ex1_figures.R`
   - `figure_5_adjusted.pdf`: Rendered by the script `example_2/ex2_figures.R`, then manually adjusted in Inkscape
- `example_1/`: Material from Worked Example 1
   - `ex1_figures.R`: Code for generating `figures/figure_4.pdf`. Sources `example_1/ex1_models.R`
   - `ex1_models.R`: Code for fitting the GLLVM models used for the final analysis in Worked example 1.
   - `data/`: Raw data for Worked Example 1
       - `Fernandez et al. 2021_JVS_Data.xlsx`: Raw data from Fernandes et al. (2021). Downloaded from: https://doi.org/10.6084/m9.figshare.17055563
- `example_2/`: Material from Worked Example 2
   - `ex2_figures.R`: Code for generating initial version of figure 5 (`figures/figure_5.pdf`). Sources `example_2/ex2_model.R`
   - `ex2_model.R`: Code for fitting the GLLVM model used for the final analysis in Worked example 2.
   - `data/`: Raw data for Worked Example 2 (Mehlhoop et al. 2022). Downloaded from: https://doi.org/10.17605/OSF.IO/6H2VJ
       - `environmental_variables.rda`: R data frame of environmental variables 
       - `species.rda`: R data frame of species observations
  
## Contact
For questions/requests about the Supporting material or the manuscript, please e-mail **audun.rugstad@ntnu.no**
