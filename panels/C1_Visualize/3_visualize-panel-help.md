<h4>Visualize</h4>
The *visualize* module gives a basic plot of the variables chosen in the input 
panel (See below). iNZight Lite automatically displays a plot tailored to the 
type of variable selected. For example, if you select two numeric variables 
(e.g. weight and height), it will display a scatterplot. On the other hand, if 
you select a numeric variable and a categorical variable (e.g. weight and 
nationality), it will display multiple box plots, depending on the number of 
categories in the categorial variable.

The old module is divided up into up to three distinct vertical *panels*:

- Panel 1: User Input (Left)
- Panel 2: Statistical Output (Middle)
- Panel 3: Advanced options (Right)

Panel 3 is only displayed if in Panel 1 the "Advanced Options" Radiobutton has 
the "show" option selected

The new module is divided up into up to two distinct vertical *panels*:

- Panel 1: User Input (Left: **Select Variables**) and Advanced options (Left: **Add To Plot**)
- Panel 2: Statistical Output (Right)

Users can switch from one module to another by clicking **REVERT TO OLD VERSION** or **REVERT TO NEW VERSION** buttons.

<br>

<h5> Panel 1: Variable selection </h5>
This panel consists of a collection of user **inputs**:

**Header** Next to the header is a checkbox which makes it possible to change 
the selection method of the two main variables. If not checked, normal dropdown 
menues can be used. When checked the variables can be selected from a list with 
scroll bars next to them. In this view it is possible to change selection with 
the arrow keys if the widget is selected.   

**Select first variable** lets you choose a variable to plot. A variable is 
chosen here by default, but you can choose a different one if you wish. The plot 
generated will either be a box plot combined with a dotplot (for numeric 
variables) or a barplot for categorical variables. In case the data is larger 
than 2000 rows the dotplot (numeric variable) becomes a histogram.

**Select second variable** lets you choose a second variable here to explore 
the relationship between two variables. The plot generated from selecting a 
second variable depends on the type of variable chosen. The combinations are 

- **numeric-numeric** Generates a scatter plot or for large data a grid-density 
plot

- **numeric-categorical** Any combination generates dotplots combined with 
boxplots seperated by the categorical variable into different sets of plots.

- **categorical-categorical** Bar plots which are subset by the second variable 
selected.  

**Subset by** gives you the option of subsetting the the variable(s) you 
chose above. If the variable you choose to subset by is not categorical, iNZight 
Lite will automatically partition your data and define a set of categories for 
you. You also have the option of subsetting by a second variable. You can adjust 
the subset *level* by adjusting the slider(s) displayed underneath the plot 
within the output panel in the middle part of the screen.

**Advanced Options** (*advanced users only*) brings up the third panel which 
makes it possible to customize the plot further. See Panel 3 below.

**Reset All** will reset all the variable selections and graphical parameters 
used to generate the plot to their default values.

<br>

<h5> Panel 2: User Output </h5>
This panel consists of a collection of tabs that display statistical output, 
either in graphical or text form:

1. **Plot** displays the plot generated based on your selections in the input 
panel on the left hand side of the screen. If you chose to subset your variables 
by one or more variables, it will display one or more sliders that lets you 
adjust the subset levels.

2. **Summary** displays a basic statistical summary of the variable(s) chosen. 
This is typically a table of counts or proportions.

3. **Inference** displays inferential information of the variable(s) chosen. 
This will typically be a 95% confidence interval as well as a chi-square test 
for equal probabilities for the variable(s).

<h5> Panel 3: Advanced Options </h5>
This panel is only visible if the "Advanced Options" radiobuton is set on show. 
It provides features to modify and add to the plot output. The panel is divided 
into two sections (Inference and Advanced options).

1. **Inference** At the top of the panel are options which will let you add 
inferential information to the plot. If both selected variables are numeric 
(scatter plot) inference can only be added if a trend curve is fitted (see 
Advanced Options below). For most plots inference for median or mean can be 
selected. Barplots and scatter plots are the only plot types where this is not 
possible. The type of inference can be selected by selecting Normal or Bootstrap 
inference and the type of the interval to be displayed can be selected 
(confidence or comparison). For histograms with boxplots, the boxplot will 
disappear when the inference parameter mean is selected. The boxplot shows the 
data according to the median and therefore adding the confidence interval on top 
would lead to misleading information. For scaterplots inference of the fitted 
trend line is displayed.  

2. **Advanced Options** Several categories of options can be selected to add 
features to the plot. Not all features are available for all types of plots. 

- **Code more variables** Enables to color the plot according to a subsetting 
column or resize the observations. If the plot become coloured, a feature in 
"Add trend curve" becomes active which makes it possible to fit trend curves 
according to the levels in the subsetting variable.

- **Add trend curve** Fits trend curves to the plot (scatterplot only). Linear, 
quadratic, cubic and/or smoother can be added. The colours of the lines can be 
specified. For the smoother, whether the quantiles should be used can be 
selected. How smoth the smother is can be adjusted and when the plot is 
coloured by some subsetting variable it is possible to fit a trend for every 
subset. Those trends can be fitted in parallel or not.

- **Add x=y line** A line where the x values equal the values in the y axis can 
be added to the plot. The colour of the line can be specified. Note, nothing 
will be displayed if the line falls outside the plot.

- **Add a jitter** To all observations small random errors are added to shift 
the observations slightly inside the plot. This can be done for the variable in 
the x axis and for the variable in the y axis. See the R function jitter in the 
base package for more information.

- **Add rugs** The distribution of the observations along the x axis and/or y 
axis can be visualized by adding rugs to the plot. For more information see the 
R function rug in the graphics package.

- **Join points by line** All the observations in the plot can be joined by 
drawing a line between them.

- **Change plot appearance** With this feature the general appearance of the 
plot can be changed. For a scatter plot a grid density or a hexbin plot can be 
displayed.  Dot plots can be changed to histograms. A selection of colours is 
available for the background or the interior of the points to be drawn. Whether 
the interior of the points is filled or not can be selected and the transparency 
of the points can be adjusted.

- **Identify points** This panel lets the user identify points of interst in the 
plot. This can be done by labeling the points and/or colouring them. The type of 
labels and the color can be changed. For labels it is possible to label by any 
column in the dataset or additionally by 'id'. The 'id' is the row number of the 
observation in the data. A third selection makes it possible to merge the 
labeled points by observatins from a different variable with the same level.
Furthermore three different selection methods can be used to label. <br>
**Select by value**. This method is used to pick observations from a list of a 
selected column. The observations are ordered and if they are not unique all 
observations with the same level are selected. A checkbox labeled "Single value" 
can be used to pick unique observations. For this the point of interst needs to 
be found by scrolling through all points on a slider or numeric input. <br>
**Extremes** With this feature extreme values can be selected. For scatter plot 
this is picking extremes by the Mahalanobis distance method. How many extreme 
points can be specified by adjusting a slider to the desired number in scatter 
plots and two numeric values can be used to label extremes in dotplots. Some 
functions such as showing the stored points does not work when "Extremes" is 
selected. <br>
**Range of values** The last method lets the user choose a range of values. A 
column can be selected and a range from this values according to the position in 
the ordered variable can be selected from a slider.

- **Customize labels** Specify x and/or y axis labels and a main title for the 
plot.

- **Adjust axis limits** This feature makes it possible to adjust the x and y 
axis labels.

