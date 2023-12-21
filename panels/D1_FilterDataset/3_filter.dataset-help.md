<h4>Filter the data on some condition</h4>
This module starts with a single drop down menu on the left and a summary of
the data on the right in the main panel. All possible different selections are
listed in the menu. when the selection is changed the left hand side updates
according to the selection. The differnt selections are listed below.
<ul>
   <li>
      <b>Filter Dataset</b> <br>
      <ul>
         <li>
            <b>levels of categorical variable: </b> A column of categorical
            variables can be selected. In a second Dropdown menu factors can be
            selected for removal.
         </li>
         <li>
            <b>numeric condition: </b> Simple numeric conditions can be
            specified to remove all rows where the selected column does not
            fulfil the specified condition.
         </li>
         <li>
            <b>row indices:</b> Paste or type in a comma seperated list of
            indices to remove from the data set. All rows at those indexes are
	    removed after the "Perform operation" button is pressed.
         </li>
	 <li>
            <b>randomly:</b> In two text fields the sample size can be specified
	    and the number of samples. A message below warns if the sample can
            not be taken. A check box can be used to sample with replacement.
            In this case the the number of samples times the sample size can be
            higher than the number of rows in the data. In any case the sample
            size must not exceed the number of rows in the data.
         </li>
      </ul>
   </li>
</ul>
