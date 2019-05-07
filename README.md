Explore Discretionary Accruals Interactively
============================================

This repository has been developed for the Panel Discussion on Replication at the [2019 JAR Conference](https://research.chicagobooth.edu/arc/journal-of-accounting-research/jar-annual-conference).

It provides the code for the [Discretionary Accruals Exploration Platform](https://jgassen.shinyapps.io/explore_dacc/). 

The display estimates Modified Jones  and Dechow and Dichev discretionary accruals and presents them with selected 
firm-year accounting data for a large panel of U.S. incorporated non-financial firms.
The methodology loosely follows [Hribar and Nichols (JAR, 2007)](https://doi.org/10.1111/j.1475-679X.2007.00259.x). 
The discretionary accruals are estimated for all Fama/French 48 industry-year 
bins with at least 10 observations available. 

See `run_me.R` for the main code and `code/pull_wrds_code.R` for the code that
pulls Compustat and Audit Analytics data from WRDS.

Edit and source `run_me.R` in a reasonably up-to-date R/RStudio environment to locally regenerate the shiny platform.

The code is based on my ['ExPanDaR' R-package](http://joachim-gassen.github.io/ExPanDaR/). 
Feel free to reach out via [email](mailto:gassen@wiwi.hu-berlin.de) or [twitter](https://twitter.com/JoachimGassen) if you happen to have remarks about this project.

Some additional links that might be of interest:

* [Explore the Preston Curve (association of national income with life expectancy)](https://jgassen.shinyapps.io/expand_rdfanalysis)

* [Explore EPA data on fuel efficiency](https://jgassen.shinyapps.io/expand_fuel_economy)

* [Explore your own data](https://jgassen.shinyapps.io/ExPanD) (No worries, your data won't be stored)

* ['ExPanDaR' R package that provides the exploration infrastructure](https://joachim-gassen.github.io/ExPanDaR)

* [R package for Researcher Degrees of Freedom Analysis](https://joachim-gassen.github.io/rdfanalysis)

* [Blog with further info - only partly related](https://joachim-gassen.github.io)

Enjoy!