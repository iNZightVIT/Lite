*Last Updated: March 10, 2015*

Introduction
-------------
The *visualize* module gives a basic plot of the variables chosen in the input panel (Panel 1 below). iNZight Lite automatically displays a plot tailored to the type of variable selected. For example, if you select two numeric variables (e.g. weight and height), it will display a scatterplot. On the other hand, if you select a numeric variable and a categorical variable (e.g. weight and nationality), it will display multiple box plots, depending on the number of categories in the categorial variable.

The module is divided up into two distinct vertical *panels*:

- Panel 1: User Input (Left)
- Panel 2: Statistical Output (Right)

Panel 1: User Input
-------------------
This panel consists of a collection of user **inputs**:

1. Select first variable
A variable is chosen here by default, but you can choose to plot a different one if you wish. The plot generated from selecting a variable will either be a box plot (for numeric variables) or a histogram (for categorial variables). 

2. Select second variable
You can choose a second variable here to explore the relationship between two variables. The plot generated from selecting a second variable depends on the type of variable chosen - read the introduction above.

3. Subset by
You can choose to subset the variable(s) chosen by the levels of the variable (need not be the same one). If the variable you choose to subset by is not categorical, iNZight Lite will automatically partition your data and define a set of categories for you. You also have the option of subsetting by a second variable. You can adjust the subset *level* by adjusting the slider(s) displayed underneath the plot within the output panel on the right hand side of the screen.

4. Advanced Options
If you are an advanced user, you can choose to customize the visual apperance of the plot by clicking "Show". Here, you can either change the apperance by adjusting the graphical parameters used in the plot, or add inferential markups to the plot. 

5. Reset All
This button restores the default values of all the variables and graphical parameters used in generating the plot displayed.

Panel 2: User Output
---------------------
This panel consists of a collection of tabs that display statistical output, either in graphical or text form:

1. Plot
This tab displays the plot generated based on your selections in the input panel on the left hand side of the screen. If you chose to subset your variables by one or more variables, it will display one or more sliders that lets you adjust the subset levels.

2. Summary
This tab displays a basic statistical summary of the variable(s) chosen. This is typically a table of counts or proportions.

3. Inference
This tab displays inferential information of the variable(s) chosen. This will typically be a 95% confidence interval and a chi-square test for equal probabilities for the variable.





