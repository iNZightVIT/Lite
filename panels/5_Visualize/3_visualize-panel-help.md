<h5> Visualize Module </h5>

Three sections are available for generating advanced plots. On the left the input parameters for changing the plot appearance. can be found. In the middle section the plot itself is shown, and on the right a menu enables to add inference lines and confidence intervals to the plot. All input menu wiggets will be explained. It is stronly recomended to follow the following tutorial to generate advanced plots.

<h6>Select first variable<h6>
When the "Graphics"" section is selected from the main navigation bar at the top of the page, this drop down menu is already filled and the first item is selected and a plot of the first column is presented in the middle section. This menu is filled with all the column names in the selected data set. Select the column in the data set which should be plotted in the area.

<h6>Subset first variable<h6>
This dropdown menu is also filled with all the column names in the selected data set. The selected value by default is "none", which is not a column in the selected data. It just means that no subsetting is selected. If a column is selected the plot is split into different sections. For factor columns the plot is split into as many levels the factor variable has and in every plot only the values selected for variable one which also have the split level are plotted. In case a numeric variable is selected the plot is always split into four plots. Each plot represents a quantile of the selected numeric column and the main text gives the range of the quantiles. When the selected data is subsetted this way another drop down menu appears. See "Subset level" below. 

<h6>Subset level<h6>
With the last drop down menu ("Subset first variable") it was possible to split a plot into sub plots of the specified levels. This menu enables to select a single plot of all of the plots variable one is split into. By default the "All" item is selected, which means that the split plot is presented.

<h6>Select second variable<h6>
If variable one and variable two is selected they are plotted as x and y in the new plot and (if selected) subsetted as selected. The deffault value for this menu is none which means only variable one is plotted. Only the columns which are not selected in variable one can be selected in variable two.

<h6>Subset second variable<h6>
The second variable also can be subset. If subsetted this way the data which is plotted is filtered by the subset column such that only those value appear in the plot which are also within the selected subset category. The category can be changed in the second drop down menu called "Subset level". 

<h6>Subset level (second)<h6>
This drop down menu appears as soon the subset for the second variable is selected. The plotted data is filtered by the level selected in this menu such that only values are plotted which are memebers of the selected item. The subset cataegories are obtained the same way as described in "Subset first variable".

<h6>Select style<h6>
Two radio buttons are provided to change the style of the plot. If "Small" (default) is selected the size of the plotindg symbols is small, otherwise (Large) big plotting symbols are used. This can be further defined in the "Change appearance of plot" section.
<h6>Change appearance of plot<h6>
This check box is by default not selected. If selected it more menu items to further change the plot. See explanations below.

<h6>Reset all<h6>
This button resets all values to default values. Only press if you want to start over.
