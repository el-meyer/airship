# AIRSHIP - An Interactive R-SHIny apP for data visualization <img src="man/figures/logo.png" align="right" width="125" alt="" />

&nbsp;

## General Information                

This app is designed to plot simulation results of clinical trials. Its main feature is allowing users to simultaneously investigate the impact of several simulation input dimensions through dynamic filtering of the simulation results. A more detailed description of the core app can be found in [Meyer et al](https://www.softxjournal.com/action/showPdf?pii=S2352-7110%2823%2900043-2). Please note that the app has evolved since the paper was published. The app is optimized for [FACTS](https://www.berryconsultants.com/software/facts/) - Fixed and Adaptive Clinical Trial Simulator - simulation output files, but can be used with any dataset in .csv format. 

The app can be installed and run locally, or accessed through the [live version](https://shiny.berryconsultants.com/airship/).

## Installation and Execution

You have a choice of installing *airship* from either CRAN or Github. Github offers both the latest stable version, as well as the development version. CRAN offers a stable version that is either consistent with the latest stable version on Github, or older.

You can install the latest stable version from [CRAN](https://cran.r-project.org/web/packages/airship/index.html) using 

``` r
install.packages("airship")
```

and run the app using:

``` r
library(airship)
airship()
```

You can install the latest stable version from [GitHub](https://github.com/el-meyer/airship) using:

``` r
# install.packages("devtools")
devtools::install_github("el-meyer/airship@*release")
```

The development version can be installed using:

``` r
# install.packages("devtools")
devtools::install_github("el-meyer/airship")
```

In some instances, the installation may fail and error with an uninformative "lazy-loading" error. Installing [Rtools](https://cran.r-project.org/bin/windows/Rtools/) may solve the problem. This is particularly true when running Windows on a VM on an ARM based machine. We have included a troubleshooting guide [here](https://el-meyer.github.io/airship/articles/IntegrationFACTS.html#troubleshooting).

## Documentation

Please see the [reference manual](https://el-meyer.github.io/airship/) for more information on the usage of this package. You can also directly jump to the [vignette](https://el-meyer.github.io/airship/articles/AIRSHIP-vignette.html) or one of the examples: [example1](https://el-meyer.github.io/airship/articles/Example1.html) and [example2](https://el-meyer.github.io/airship/articles/Example2.html).

## Funding

Elias Laurin Meyer is a salaried employee of [Berry Consultants](https://www.berryconsultants.com/) and was previously a member of the EU-PEARL consortium. 

EU-PEARL (EU Patient-cEntric clinicAl tRial pLatforms) project has received funding from the Innovative Medicines Initiative (IMI) 2 Joint Undertaking (JU) under grant agreement No 853966. This Joint Undertaking receives support from the European Union’s Horizon 2020 research and innovation program and EFPIA and Children’s Tumor Foundation, Global Alliance for TB Drug Development non-profit organization, Springworks Therapeutics Inc. This publication reflects the authors’ views. Neither IMI nor the European Union, EFPIA, or any Associated Partners are responsible for any use that may be made of the information contained herein.


