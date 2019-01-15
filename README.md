# What is "Mar.JoyofQC"?

This is an app that facilitates the visualization of data.  It helps users identify and flag outliers in their r, csv and oracle datasets.

## Select Data

The data to be examined is chosen within the "Select Data" tab.  It can be either a local R object, a csv on your computer, or an Oracle table you already have access to.'
If you choose a csv or an r object, you will be prompted to select it.  If you are working with Oracle, you will first need to provide your Oracle credentials, then you will choose from the schema(s) you have access to, and then select one of the tables from within that schema.
Once you've selected a dataset, 3 new dropdown boxes will appear, and each will be populated with all of the fields available from the dataset.  These include the options for choosing the x-axis and y-axis of the ensuing plots. Additionally, you can select an (optional) 'facet' field.  By faceting the data, you will generate a seperate plot for each unique value in the facet field.  For example, you might choose to plot 'length' vs 'weight' (as the x and y axes), but then elect to the facet the plots by 'species'.  The would results in a seperate length vs weight plot for each species.

## Results
This is the meat of the application, and the place where you data will be displayed in graphical format. At the top of the page will be some tools that allow you to interact with the plots.  Below that is one or more plots of the data (depending on whether or not you elect to facet your data).  At the bottom is a table that gets populated with the data you've selected")

### Plot Tools

The plots are dynamic, in that you can draw boxes on them to select sets of data.  The first row of tools work on the selections you make, and allow you to flag them as 'good' or 'bad', and (optionally) add a comment describing why you feel that way. Additionally, selected data can simply be hidden rather than have a QC status applied. The second row of tools relates to saving the results of your QC session, and allows you to save either the entire dataset, or just the records you have flagged as good or bad.  Lastly, depending on the facet selections, you may get a warning that too many plots will be generated to be useful.  Should this be the case, a button will be presented allowing users to override the warning and show al of the plots regardless.
