Time Series Module
===================

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


Panel 1: User Input
-------------------
This panel consists of a collection of user **inputs**. It contains the following options:

1. Time Information
Users must either choose a time variable from a list of all the variables (or column names) from the selected dataset, or define one manually. For more information on how to define time information manually, please refer to https://www.stat.auckland.ac.nz/~wild/iNZight/faq.php#time-series.

2. Seasonal Pattern
Next, it is necessary to choose how the seasonal pattern is defined. It is standard to use the multiplicative scale here. For more detailed information on what seasonal patterns mean, please refer to the *Time series decomposition* section of https://www.otexts.org/fpp/6/1.

3. Series Variables
Finally, users must select variables to plot over time. If multiple variables are selected, the output panel automatically displays a multiple-series plot. Otherwise, a single-series plot will be displayed.

4. Label Customization (Optional)
Users can manually provide labels for the x and y axes if they wish.


Panel 2: User Output
--------------------
This panel consists of a collection of tabs that display statistical output, either in graphical or text form. The tabs displayed are as follows (from left to right):

1. Time
This tab displays the time plot, which is a simple line plot of the variables selected in 1.3 (on the y-axis) against the time variable (on the x-axis).

2. Seasonal
This tab displays the seasonal plot for the variable(s) chosen on the left hand side, and a plot of the decomposed seasonal (multiplicative or additive) effect on the right hand side.

3. Decomposed

4. Recomposed

5. Forecast

6. Summary

E-mail *Chris Park* <cpar137@aucklanduni.ac.nz> 
