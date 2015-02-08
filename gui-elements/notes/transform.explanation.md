<h4>How to use the transform panel?</h4>
 <ol>
  <li>Select one or more columns:<br>Using the dropdown menu at the left hand side labeled "Select Columns" select any number of columns available. The selected columns should be displayed in the middle of the screen.</li>
  <li>Select transformation type:<br>Select any (one) of the transformation types in the dropdown menu labeled "Select transformation:". After the selection, an additional number of columns is added to the presented data. </li>
  <li>Transform columns:<br>Step one and two only presented the data and help with the selection. Until now no additional data was added. To add the additional columns to your data, press the "Transform" button. After that the additional transformed columns are added and will be available.</li>
</ol>
<h4>Add Transformation</h4>
Adds up all selected columns which are numeric.
<h4>Subtract Transformation</h4>
Subtracts all selected columns which are numeric such that "A-B-C". Where A, B and C are the selected columns.
<h4>Multiply Transformation</h4>
Multiplies all selected columns which are numeric.
<h4>Divide Transformation</h4>
Divides all selected columns which are numeric.
<h4>Log Transformation</h4>
Performs a log transformation of all selected columns. By default this is the natural log. For more information please look at the R function "log".
<h4>Root Transformation</h4>
If the columns are of class numeric the square root of each value is estimated and returned.
<h4>Square Transformation</h4>
All columns of class type numeric are squared, such that X*X.
<h4>Abs Transformation</h4>
Of all numeric columns from the selected column the absolute value is returned. See the "abs" function in R for more information.
<h4>Center Transformation</h4>
All columns (character, factor and numeric) are centered to mean 0. Such that from column X the mean of X is subtracted from X. Character and factor values are converted to numeric such that whith the order of the factors values from 1 to the nuber of unique factors are asigned to each value in the column.
<h4>Standardize Transformation</h4>
In a first step exactly tha same method is performed as described in "Center Transformation" above. As an additional step the result from the first step is divided by the standard deviation of the column.
<h4>Median Split Transformation</h4>
All numeric selected columns are converted into a binary variable such that all values above the median are classified as "high" and all values below the median are classified as "low".
<h4>Reverse-coding Transformation</h4>
The selected numeric columns are revers coded such that the minimum and maximum of Column X is added and X is subtracted from this value ((min(X)+max(X))-X).  
<h4>Copy Transformation</h4>
Copies the selected coulms so that they become duplicates in the current data set.
<h4>Change sign Transformation</h4>
Performs the action X*-1 such that all negative values become positive and all positive values become negative.
<h4>Change factor Transformation</h4>
Changes any variable/column to a factor value. Note, all character columns are already treated as factor.
