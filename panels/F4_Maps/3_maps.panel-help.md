<h4> Introduction </h4>

The Maps Module aims at making it easy to visualise geographical data on a map, and investigate patterns of variables. To use the Maps module in iNZight Lite, one must first select or upload a dataset.

The module is divided up into two distinct vertical *panels*:

- Panel 1: User Input (Left)
- Panel 2: Maps Plot (Right)

<br>

<h4> Panel 1: User Input </h4>
The panel is divided up into two sub-panels:

- <h5>sub-Panel 1: Select Variables</h5>

There are two situations that iNZight Lite can handle:

**1. Each observation has a co-ordinate value:** You must make sure the **Coordinate** option is selected, and then specify the **Latitude** and **Longitude** variables. iNZight Lite will try to guess **Latitude** and **Longitude** automatically, but you might need to select them manually if they have nonstandard names.

**2. The data are associated with regions (such as Countries, states, etc):** You must select the **Regions** option, and then specify the **Map Location** and **Location Variable** variables. **Map Location** indicates the overall location of your data and **Location Variable** tells iNZight Lite which variable in the dataset contains the geographical location. After that, in order to generate a map and display your data, you need to select a variable from the dropdown at **Plotting Variable**.

You can also view multiple subsets easily by selecting one or two sub-setting variable(s) from the dropdown at **Select subset variable 1** and **Select subset variable 2**.

- <h5>sub-Panel 2: More Options</h5>

**1. Coordinate Selected**

- **Colour by:** points will be coloured by the chosen variable.
- **Size by:** the area of each point will be proportional to the value of the chosen variable.
- **Opacify by:** the opacity (how visible the points are) will be proportional to the value of the selected variable.
- **Map type:** iNZight Lite Maps uses Google to provide the map, of which there are several type to choose from "roadmap ", "satellite ", "terrain " and "hybrid ". 
- **Colour:** the colour of points (only if **Colour by** is empty).
- **Point size:** the overall size of points.
- **Transparency:** the overall transparency of points, where 0 is fully visible, to 1 is fully invisible.
- **Connect points by lines:** points will be connected by lines.

**2. Regions Selected**

- **Colour:** the colour used on the map. 
- **Missing value colour:** the colour used to shade regions with a missing value.
- **Plot labels:** add labels to the plot. The options include Name, which adds the region names, Value, which adds the value of the chosen variable, and Both, which adds the Name and Value.


<br>

<h4> Panel 2: Maps Plot </h4>
This panel displays the maps plot generated based on your selections in the input panel on the left hand side of the screen. If you chose to subset your variables by one or more variables, it will display one or more sliders that lets you adjust the subset levels.

