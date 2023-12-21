<h4>Model Fitting</h4>
This module is designed to fit regression models from the data. The controls on
the left are divided into six sections. The Main panel on the right shows model
summaries plots or code to produce the models.
- **Select model** In the first section all models are stored. From a drop down
menu a model can be selected, renamed with a text field next to it by typing in
a new name and pressing the "RENAME" button. The "REMOVE" button removes a model
from the list. There is no model to choose from by default. Underneath the model
controls is a select bar which lets the user switch between the "Model" summary,
"Plots" or the "Code History".
<h5>Model</h5>
- **Choose Model settings** This seetings makes it possible to select the Y
variable from the data. Transform the Y variable by log, sqare root or raising
it by a factor which can be specified in a text filed appearing in the same row.
The model framework can be selected and whether the design is a complex survey
or not.
-**Predictor variables** The next section is for choosing predictor variables or
confounding variables.
- **Interaction terms** Simple interactions can be specified with the next
section. The choices are "All" (All possible interactions), "by degree" (in a
text field the degree of interaction is supplied), "by variable" (specific
interaction between variables).
- **Transform x variables** Different transformations of the x variables can be
specified.
- **Display Formula** The code which will produce the module is presented in
this section.
<h5>Plots</h5>
Three different plot categories can be selected.
- **Factor level comparison** This plot can be produced for every factor in the
fitted model. It shows how each factor for a variable influenes the model.
- **Graphical diagnostics** Six different plots can viewed. A plot of the
residuals versus the fitted values, a scale location plot, The residuals versus
leverage, a cooks distance plot, a normall QQ-plot and a histogram of the
residuals.
- **Normality checks** The Normal QQ-plot is repeated here and the histogram as
well as a histogram of sampled data and a QQ-plot including inference.
<h5>Code History</h5>
The R code which produced the models can be viewed and downloaded.<br>
