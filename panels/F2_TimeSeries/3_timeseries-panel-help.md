<h4> Introduction </h4>
In statistics, one often wishes to gain insight from data gathered over a period of time. *Time series* refer to data collected sequentially over time. Time series arise in a wide variety of fields, including:

- Financial forecasting
- Stock market analysis
- Weather forecasting
- Medical research

Time series analysis is useful, as it allows one to detect patterns from the past to predict the future.

To use the Time Series module in iNZight Lite, one must first choose a dataset with a well-defined "time" column. iNZight Lite can currently only handle datasets with a monthly/yearly time format (additional format capabilities will be added on throughout 2015).

The module is divided up into two distinct vertical *panels*:

- Panel 1: User Input (Left)
- Panel 2: Statistical Output (Right)

<br>

<h4> Panel 1: User Input </h4>
This panel consists of a collection of user **inputs**:

1. **Time Information**: You must either choose a time variable from a list of all the variables (or column names) from the selected dataset, or define one manually. If you time variable is not in the format the iNZight Lite recognises, then you must supply the time information manually (see https://www.stat.auckland.ac.nz/~wild/iNZight/faq.php#time-series).

2. **Seasonal Pattern**: You must choose how you would like the seasonal pattern to be defined. It is standard to use the multiplicative scale here. For more detailed information on what seasonal patterns mean, please refer to the *Time series decomposition* section of https://www.otexts.org/fpp/6/1.

3. **Series Variables**: Finally, you need to select variables to plot over time. If multiple variables are selected, the output panel automatically displays a multiple-series plot. Otherwise, a single-series plot will be displayed.

4. **Customize Labels** (Optional): You can manually provide labels for the x and y axes if you wish.

<br>

<h4> Panel 2: User Output </h4>
This panel consists of a collection of tabs that display statistical output, either in graphical or text form:

1. **Time** displays the time plot for the chosen variable(s), which is a simple line plot of the variables selected in 1.3 (on the y-axis) against the time variable (on the x-axis).

2. **Seasonal** displays a seasonal plot for the chosen variable(s), and a plot of the decomposed seasonal (multiplicative or additive) effect on the right hand side.

3. **Decomposed** displays a decomposed plot for the chosen variable(s).This plot displays, from top to bottom, the trend, seasonal swing, and residuals of the chosen variable.

4. **Recomposed** displays a recomposed plot for the chosen variable(s).This plot displays, from top to bottom, the trend + seasonal, seasonal swing, and residuals of the chosen variable.

5. **Forecast** displays a Holt-Winters prediction plot for the variable(s) chosen in the input panel

6. **Summary** displays a statistical summary (typically a confidence interval) of the chosen variable.

<br>

