<h5>Aggregate the data</h5>
Two select boxes on the left of panel enable to select one or more categorical 
variables from the current dataset. The function selected in the second select 
box is applied on all numeric variables over all possible combinations of the 
selected categorical variables. 
More than one method can be selected.<br><br>
Methods:<br>
 <ul>
  <li>
    mean<br>
    The R function "mean" is used to calculate the mean of the values in the 
    combination of selected data categories.
  </li>
  <li>
    median<br>
    The R function "median" is used to calculate the median of the values in the 
    combination of selected data categories.
  </li>
  <li>
    sd<br>
    The R function "sd" is used to calculate the standard deviation of the 
    values in the combination of selected data categories.
  </li>
  <li>
    IQR<br>
    The R function "IQR" is used to calculate the interquartile range of the 
    values in the combination of selected data categories. The default method is 
    used to perform this.
  </li>
  <li>
    count<br>
    The R function "length" is used to count the values in the combination of 
    selected data categories.
  </li>
</ul>
For more information see the R help on the described methods.

