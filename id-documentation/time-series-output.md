# outputID's for Time Series  
<!-- dilinger.io -->
> This is a list of all the **outputId**'s used in the **Time Series** module for [iNZight Online], in the order that they appear in the module UI. Please e-mail [Chris Park] for any questions and/or suggestions.  

#### Group A: Data Validation
___
- **validate**: *textOutput* for checking if the data is in a suitable format.

#### Group B: Input Summary
___
- **currentTime**: *textOutput* for displaying the current time.

 
#### Group C: Single-Series Plots 
___

- **timeseriesPlot**: *plotOutput* for the time series plot.
- **seasonalPlot**: *plotOutput* for the seasonal plot.
- **decomposedPlot**: *plotOutput* for the decomposed plot.
- **trSeasonalPlot**: *plotOutput* for the trend + seasonal plot.
- **forecastPlot**: *plotOutput* for the forecast plot.


#### Group D: Several-Series Plots
___
- **single.plot.layout**: *uiOutput* for the single-plot option.
- **multi.plot.layout**: *uiOutput* for the multi-plot option.


###### Date created: January 15, 2015.
[iNZight Online]: http://docker.stat.auckland.ac.nz
[Chris Park]: cpar137@aucklanduni.ac.nz