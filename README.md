# AIRSHIP - An Interactive R-SHIny apP for visualizing tidy long data

## General Information                

This app is designed to plot simulation results of clinical trials. Its main feature is allowing users to simultaneously investigate the impact of several simulation input dimensions through dynamic filtering of the simulation results. A more detailed description of the app can be found in [Meyer et al](https://www.softxjournal.com/article/S2352-7110(23)00043-2/fulltext).

## Installation and Execution

You can install the latest stable version from [GitHub](https://github.com/el-meyer/airship) using:

``` r
# install.packages("devtools")
devtools::install_github("el-meyer/airship@*release")
```

and run the app using:

``` r
library(airship)
airship()
```

The development version can be installed using:

``` r
# install.packages("devtools")
devtools::install_github("el-meyer/airship")
```

## Documentation

Please see the [reference manual](https://el-meyer.github.io/airship/) for more information on the usage of this package, or jump directly to the [vignette](https://el-meyer.github.io/airship/articles/AIRSHIP-vignette.html).

## Funding

Development is currently funded by [Berry Consultants](https://www.berryconsultants.com/) and was previously funded by EU-PEARL. 

EU-PEARL (EU Patient-cEntric clinicAl tRial pLatforms) project has
received funding from the Innovative Medicines Initiative (IMI) 2 Joint
Undertaking (JU) under grant agreement No 853966. This Joint Undertaking
receives support from the European Union’s Horizon 2020 research and
innovation program and EFPIA and Children’s Tumor Foundation, Global
Alliance for TB Drug Development non-profit organization, Springworks
Therapeutics Inc. This publication reflects the authors’ views. Neither
IMI nor the European Union, EFPIA, or any Associated Partners are
responsible for any use that may be made of the information contained
herein.


