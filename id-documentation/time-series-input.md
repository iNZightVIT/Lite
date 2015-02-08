# inputID's for Time Series  
<!-- dilinger.io -->
> This is a list of all the **inputId**'s used in the **Time Series** module for [iNZight Online], in the order that they appear in the module UI. Please e-mail [Chris Park] for any questions and/or suggestions.  

#### Group A: Time information
___
- **timeinfo**: *radioButtons* for defining the time variable. Either a variable from a list (input.timeinfo == 1), or defined manually (input.timeinfo == 2). 

```R
###  inputId conditional on input.timeinfo == 1
```
- **timevars**: *selectInput* for choosing the time variable from a list.

```R 
###  inputId's conditional on input.timeinfo == 2
```
- **defineStart**: *textInput* for specifying the start date.
- **defineSeason**: *textInput* for specifying the season.
- **defineFreq**: *textInput* for specifying the frequency.

#### Group B:  Seasonal Pattern
___
- **season**: *radioButtons* for defining the seasonal pattern. Either multiplicative (default) or additive.

####  Group C: Series Type
___

- **series**: *radioButtons* that specifies the type of series. Either single (input.series == 1) or several (input.series == 2). 

```R 
###  inputId's conditional on input.series == 1
```

- **singSeries**: *checkboxGroupInput* for producing single-series plots. The possible choices are:
 1. Time Series, 
 2. Seasonal Plot,
 3. Decomposed Plot,
 4. Trend + Seasonal Plot,
 5. Forecast Plot.
 
- **singleVar**: *selectInput* for choosing the plot variable.

```R
###  inputId's conditional on input.series == 2    
```
- **sevSeries**: *checkboxGroupInput* for displaying serveral-series plots. The possible choices are:
 1. Single Plot,
 2. Multiple Plots.

- **severalVar**: *selectInput* for choosing the plot variable**s**.

####  Group D: Customize Plot
___

- **xlab**: *textInput* for labelling the *x*-axis.
- **ylab**: *textInput* for labelling the *y*-axis.
 

###### Date created: January 15, 2015.
[iNZight Online]: http://docker.stat.auckland.ac.nz
[Chris Park]: cpar137@aucklanduni.ac.nz

# inputID's for Time Series  
<!-- dilinger.io -->
> This is a list of all the **inputId**'s used in the **Time Series** module for [iNZight Online], in the order that they appear in the module UI. Please e-mail [Chris Park] for any questions and/or suggestions.  

#### Group A: Time information
___
- **timeinfo**: *radioButtons* for defining the time variable. Either a variable from a list (input.timeinfo == 1), or defined manually (input.timeinfo == 2). 

```R
###  inputId conditional on input.timeinfo == 1
```
- **timevars**: *selectInput* for choosing the time variable from a list.

```R 
###  inputId's conditional on input.timeinfo == 2
```
- **defineStart**: *textInput* for specifying the start date.
- **defineSeason**: *textInput* for specifying the season.
- **defineFreq**: *textInput* for specifying the frequency.
- **provide**: *actionButton* for specifying the above.

#### Group B:  Seasonal Pattern
___
- **season**: *radioButtons* for defining the seasonal pattern. Either multiplicative (default) or additive.

####  Group C: Series Type
___

- **series**: *radioButtons* that specifies the type of series. Either single (input.series == 1) or several (input.series == 2). 

```R 
###  inputId's conditional on input.series == 1
```

- **singSeries**: *checkboxGroupInput* for producing single-series plots. The possible choices are:
 1. Time Series, 
 2. Seasonal Plot,
 3. Decomposed Plot,
 4. Trend + Seasonal Plot,
 5. Forecast Plot.
 
- **singleVar**: *selectInput* for choosing the plot variable.

```R
###  inputId's conditional on input.series == 2    
```
- **sevSeries**: *checkboxGroupInput* for displaying serveral-series plots. The possible choices are:
 1. Single Plot,
 2. Multiple Plots.

- **severalVar**: *selectInput* for choosing the plot variable**s**.

####  Group D: Customize Plot
___

- **xlab**: *textInput* for labelling the *x*-axis.
- **ylab**: *textInput* for labelling the *y*-axis.
 

###### Date created: January 15, 2015.
[iNZight Online]: http://docker.stat.auckland.ac.nz
[Chris Park]: cpar137@aucklanduni.ac.nz

