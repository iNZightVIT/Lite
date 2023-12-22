# inputID's for Time Series

<!-- dilinger.io -->

> This is a list of all the **inputId**'s used in the **Time Series** module for [iNZight Online], in the order that they appear in the module UI. Please e-mail [Chris Park] for any questions and/or suggestions.

#### Group A: Time information

---

- **timeinfo**: _radioButtons_ for defining the time variable. Either a variable from a list (input.timeinfo == 1), or defined manually (input.timeinfo == 2).

```R
###  inputId conditional on input.timeinfo == 1
```

- **timevars**: _selectInput_ for choosing the time variable from a list.

```R
###  inputId's conditional on input.timeinfo == 2
```

- **defineStart**: _textInput_ for specifying the start date.
- **defineSeason**: _textInput_ for specifying the season.
- **defineFreq**: _textInput_ for specifying the frequency.

#### Group B: Seasonal Pattern

---

- **season**: _radioButtons_ for defining the seasonal pattern. Either multiplicative (default) or additive.

#### Group C: Series Type

---

- **series**: _radioButtons_ that specifies the type of series. Either single (input.series == 1) or several (input.series == 2).

```R
###  inputId's conditional on input.series == 1
```

- **singSeries**: _checkboxGroupInput_ for producing single-series plots. The possible choices are:

1.  Time Series,
2.  Seasonal Plot,
3.  Decomposed Plot,
4.  Trend + Seasonal Plot,
5.  Forecast Plot.

- **singleVar**: _selectInput_ for choosing the plot variable.

```R
###  inputId's conditional on input.series == 2
```

- **sevSeries**: _checkboxGroupInput_ for displaying serveral-series plots. The possible choices are:

1.  Single Plot,
2.  Multiple Plots.

- **severalVar**: _selectInput_ for choosing the plot variable**s**.

#### Group D: Customize Plot

---

- **xlab**: _textInput_ for labelling the _x_-axis.
- **ylab**: _textInput_ for labelling the _y_-axis.

###### Date created: January 15, 2015.

[iNZight Online]: http://docker.stat.auckland.ac.nz
[Chris Park]: cpar137@aucklanduni.ac.nz

# inputID's for Time Series

<!-- dilinger.io -->

> This is a list of all the **inputId**'s used in the **Time Series** module for [iNZight Online], in the order that they appear in the module UI. Please e-mail [Chris Park] for any questions and/or suggestions.

#### Group A: Time information

---

- **timeinfo**: _radioButtons_ for defining the time variable. Either a variable from a list (input.timeinfo == 1), or defined manually (input.timeinfo == 2).

```R
###  inputId conditional on input.timeinfo == 1
```

- **timevars**: _selectInput_ for choosing the time variable from a list.

```R
###  inputId's conditional on input.timeinfo == 2
```

- **defineStart**: _textInput_ for specifying the start date.
- **defineSeason**: _textInput_ for specifying the season.
- **defineFreq**: _textInput_ for specifying the frequency.
- **provide**: _actionButton_ for specifying the above.

#### Group B: Seasonal Pattern

---

- **season**: _radioButtons_ for defining the seasonal pattern. Either multiplicative (default) or additive.

#### Group C: Series Type

---

- **series**: _radioButtons_ that specifies the type of series. Either single (input.series == 1) or several (input.series == 2).

```R
###  inputId's conditional on input.series == 1
```

- **singSeries**: _checkboxGroupInput_ for producing single-series plots. The possible choices are:

1.  Time Series,
2.  Seasonal Plot,
3.  Decomposed Plot,
4.  Trend + Seasonal Plot,
5.  Forecast Plot.

- **singleVar**: _selectInput_ for choosing the plot variable.

```R
###  inputId's conditional on input.series == 2
```

- **sevSeries**: _checkboxGroupInput_ for displaying serveral-series plots. The possible choices are:

1.  Single Plot,
2.  Multiple Plots.

- **severalVar**: _selectInput_ for choosing the plot variable**s**.

#### Group D: Customize Plot

---

- **xlab**: _textInput_ for labelling the _x_-axis.
- **ylab**: _textInput_ for labelling the _y_-axis.

# outputID's for Time Series

<!-- dilinger.io -->

> This is a list of all the **outputId**'s used in the **Time Series** module for [iNZight Online], in the order that they appear in the module UI. Please e-mail [Chris Park] for any questions and/or suggestions.

#### Group A: Data Validation

---

- **validate**: _textOutput_ for checking if the data is in a suitable format.

#### Group B: Input Summary

---

- **currentTime**: _textOutput_ for displaying the current time.

#### Group C: Single-Series Plots

---

- **timeseriesPlot**: _plotOutput_ for the time series plot.
- **seasonalPlot**: _plotOutput_ for the seasonal plot.
- **decomposedPlot**: _plotOutput_ for the decomposed plot.
- **trSeasonalPlot**: _plotOutput_ for the trend + seasonal plot.
- **forecastPlot**: _plotOutput_ for the forecast plot.

#### Group D: Several-Series Plots

---

- **single.plot.layout**: _uiOutput_ for the single-plot option.
- **multi.plot.layout**: _uiOutput_ for the multi-plot option.

###### Date created: January 15, 2015.

[iNZight Lite]: http://docker.stat.auckland.ac.nz
[Chris Park]: cpar137@aucklanduni.ac.nz
