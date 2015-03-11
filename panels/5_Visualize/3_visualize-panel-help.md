#### Introduction
The *visualize* module gives a basic plot of the variables chosen in the input panel (Panel 1 below). iNZight Lite automatically displays a plot tailored to the type of variable selected. For example, if you select two numeric variables (e.g. weight and height), it will display a scatterplot. On the other hand, if you select a numeric variable and a categorical variable (e.g. weight and nationality), it will display multiple box plots, depending on the number of categories in the categorial variable.

The module is divided up into two distinct vertical *panels*:

- Panel 1: User Input (Left)
- Panel 2: Statistical Output (Right)

#### Panel 1: User Input
This panel consists of a collection of user **inputs**:

1. **Select first variable** lets you choose a variable to plot. A variable is chosen here by default, but you can choose a different one if you wish. The plot generated will either be a box plot (for numeric variables) or a histogram (for categorial variables). 

2. **Select second variable** lets you choose a second variable here to explore the relationship between two variables. The plot generated from selecting a second variable depends on the type of variable chosen - read the introduction above.

3. **Subset by** gives you the option of subsetting the the variable(s) you chose above by a variable (need not be the same one). If the variable you choose to subset by is not categorical, iNZight Lite will automatically partition your data and define a set of categories for you. You also have the option of subsetting by a second variable. You can adjust the subset *level* by adjusting the slider(s) displayed underneath the plot within the output panel on the right hand side of the screen.

4. **Advanced Options** (*advanced users only*) allows you to customize the visual appearance or add inferential markups on top of the plot. 
5. **Reset All** will reset all the variable selections and graphical parameters used to generate the plot to their default values.

#### Panel 2: User Output
This panel consists of a collection of tabs that display statistical output, either in graphical or text form:

1. **Plot** displays the plot generated based on your selections in the input panel on the left hand side of the screen. If you chose to subset your variables by one or more variables, it will display one or more sliders that lets you adjust the subset levels.

2. **Summary** displays a basic statistical summary of the variable(s) chosen. This is typically a table of counts or proportions.

3. **Inference** displays inferential information of the variable(s) chosen. This will typically be a 95% confidence interval as well as a chi-square test for equal probabilities for the variable(s).

If you find any errors / typos or have any suggestions, please e-mail them to Chris Park (<cpar137@aucklanduni.ac.nz>).

*Last Updated: March 10, 2015*
