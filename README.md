Explore Discretionary Accruals Interactively
============================================

This repository provides the code for the [Discretionary Accruals Exploration Platform](https://jgassen.shinyapps.io/explore_dacc/). It estimates Modified Jones 
and Dechow and Dichev discretionary accruals and presents them with selected 
firm-year accounting data for a large panel of U.S. incorporated non-financial firms.
The methodology loosely follows [Hribar and Nichols (JAR, 2007)](https://doi.org/10.1111/j.1475-679X.2007.00259.x). 
The discretionary accruals are estimated for all Fama/French 48 industry-year 
bins with at least 10 observations available. 

See `run_me.R` for the main code and `code/pull_wrds_code.R` for the code that
pulls Compustat and Audit Analytics data from WRDS.

Edit and source `run_me.R` to locally regenerate the shiny platform.

The code is based on my ['ExPanDaR' R-package](http://joachim-gassen.github.io/ExPanDaR/). 
Feel free to reach out via [email](mailto:gassen@wiwi.hu-berlin.de) or [twitter](https://twitter.com/JoachimGassen) if you happen to have remarks about this project.

Enjoy!