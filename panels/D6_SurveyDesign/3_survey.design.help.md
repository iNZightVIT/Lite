<h5>Survey design</h5>
This module is still under development. Some features of iNZight lite will not
work when it is used. Every design specified is connected to the data set
selected at the time the design was created. Make sure the right data is
selected first. If the data set is changed the design is deleted and needs to be
recreated. <br><br>
The survey design module adds or removes a survey design design to the vizualize
modules plotting functionality.<br>
To use this module a survey data set should be loaded. There are four dropdown
menues and one check box.
- **strata** The first dropdown menu specifies the strata. Select a variable
from the list.
- **clustering** The second dropdown menu specifies the clustering variable or
id variable in the survey design function (svydesign) More than one variable can
be selected. The "none" variable which is selected as default is removed from
the selection internaly and can be ignored. All selected variables are pased
into the "svydesign" function as formula. The format is "~var1 + var2 + ...".
- **weighting** The weighting variable is the third variable. Please specify on
what variable the survey design is weighted.
- **nest** Whether it is a nested design or not can be specified with this check
box.
- **fpc** The finite population correction variables are selected with the last
dropdown menu. As in **clustering** more than one variable can be selected. In
fact the number of variables selected for fpc should equal the number of
variables selected in clustering. <br><br>
For more information please look at the R survey package.
