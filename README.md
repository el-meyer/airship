# AIRSHIP - An Interactive R-SHIny apP for visualizing tidy long data

## Info                

This app is designed to plot simulation results of clinical trials. It has been developed by Elias Laurin Meyer, Constantin Kumaus and Michal Majka and was published in [SoftwareX](https://www.softxjournal.com/article/S2352-7110(23)00043-2/fulltext).

## User Manual

In the following you will find some details on every part of the app and how they are supposed to be used.

### Data Settings

There are a few requirements to the data in order for the app to work. So far only .csv files can be uploaded. It is expected that the data is arranged in a way such that the design parameters precede the output values/operating characteristics. Each row represents one  simulation run with a different combination of input/design parameters. 

If your data is not aggregated yet (i.e. if you have every single simulation outcome as one row in your dataset, and a 'replication run index' you can click the checkbox and choose which of your variables is the 'replication run index'. The dataset is then averaging over the OCs either by mean or median. Additionally the 'Distribution' tab opens where you can investigate the behaviour of your variables and outcomes.
                
### Data

In the Data tab you find an overview of your data. Already here you can set filters for your input parameters, if you are not interested in some observations.

### Default values

The default value is a key tab in this App. Please choose one default value for every variable that has an impact on your simulation. Later in the plot tab the dataset is filtered for these values, unless the respective variable is chosen to be one of the dimensions in the graph (See 'plot' tab).

### Distribution

This tab only appears when the checkbox regarding 'replication run index/variables' is checked. You can create boxplots or distribution plots.

                
### Plot

After uploading the data and establishing the settings, you can visualize your simulation results on up to 4 dimensions. An x-axis variable as well as at least one OC have to be specified in order for the plot to show up: You can opt to add further design parameters on the 'facet' dimensions (row and column), which splits the plot into a grid as well as the 'shape' dimension, which adds lines/points in different shapes according to the value of the respective input parameter.

Furthermore you can change the style of your plot when clicking the 'style options' button and download a plot in the exact size and quality you need when clicking the 'Download plot' button.
                
### Scatterplot

If you are interested in the variability of ceratin operating characteristics in 1 specific scenario, you can look at the settings in this tab which generates a scatterplot of 2 output variables, with the possibility of adding a grid. This is especially suitable if you ran e.g. 10000 simulation runs with the same setting and have not aggregated your data yet. Then you can choose your 'replication index variable' and investigate the variability of the outcome.


## Disclaimer

EU-PEARL (EU Patient-cEntric clinicAl tRial pLatforms) project has
received funding from the Innovative Medicines Initiative (IMI) 2 Joint
Undertaking (JU) under grant agreement No 853966. This Joint Undertaking
receives support from the European Union’s Horizon 2020 research and
innovation program and EFPIA and Children’s Tumor Foundation, Global
Alliance for TB Drug Development non-profit organization, Springworks
Therapeutics Inc. This publication reflects the authors’ views. Neither
IMI nor the European Union, EFPIA, or any Associated Partners are
responsible for any use that may be made of the information contained
herein. The research of Elias Laurin Meyer was funded until 11/2020 by
Novartis through the University and not at an individual level.


