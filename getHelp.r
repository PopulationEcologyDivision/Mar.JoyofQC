getHelp<-function(){
  return(tagList(div(id = "helpText", h3('What is "Mar.JoyofQC"?')
                 ,p("This is a Shiny app that facilitates the visualization of data.  
                    It's purpose is to support the identification and flagging 
                    of outliers in r objects, *.csv files and oracle tables.")
                 ,h4("How do I See My Data?")
                 ,p("On the left, there are controls for selecting and working with
                    datasets. As you make choices, additional, relevant options
                    will become available.  Datasets can be local R objects, csv files on your 
                    computer, or Oracle tables you already have access to.  If you 
                    choose a csv or an r object, you will be prompted to select it.  
                    If you are working with Oracle, you will first need to provide 
                    your Oracle credentials, then you will choose from the schema(s) 
                    you have access to, and then select one of the tables from within 
                    that schema.")
                  ,p("Regardless of the datasource selected, 3 options become 
                    available - 'x-axis', 'y-axis' and 'facet'.  X- and y-axis 
                    selections simply control which field is plotted against which 
                    other field.   Selecting a field to facet by results in the grouping 
                    of data by shared values from the facet field.  For example, 
                    you might choose to plot 'length' vs 'weight' (as the x and 
                    y axes), but then facet the plots by 'species'.  The would 
                    result in a separate length vs weight plot for each species.")
                 ,h4("How Can I QC My Data?")
                 ,p("As soon as you have selected an x- and y-axis, your data will 
                  be plotted (and if you choose to facet your data, you will see 
                  multiple plots).  The plots are dynamic, in that you can draw 
                  boxes on them to select sets of data.  The underlying values of the selected data is 
                  always shown below the plot(s).The first row of tools at the 
                  top of the page work on the selected data, and allow you to flag data as
                  'good' or 'bad', and (optionally) add a comment describing why 
                  you feel that way.  The QC_STATUS (i.e. good or bad) and the 
                  values for QC_COMMENT will all be included in your output.")
                 ,p("Additionally, selected data can simply be hidden rather than 
                  having a QC status applied. This can be useful if the presence 
                  of some data if causing the majority of the data to be 'squashed' 
                  near the origin.  Choosing to hide the data doesn't apply a QC 
                  status, but simply results in a button appearing that will allow
                  you to unhide the data.")
                 ,h4("How Can I Save My Work?")
                 ,p("The second row of tools above the plot(s) relate to saving 
                  the results of your QC session, and allow you to save every record 
                  (plus the additional columns 'QC_STATUS' and 'QC_COMMENT'), or 
                  just the records you have flagged as good or bad in QC_STATUS.
                  If possible, not all columns will be saved - if there is a unique 
                  field in your data, it alone will be saved with the QC results.  
                  The purpose of this application is not to generate new datasets, 
                  but to allow you to go back to original datasets and fix values 
                  there.  Data is saved both as an R object to local environment, 
                  as well as a csv to your working directory.") 
                 ,h4("Facet Notes")
                 ,p("Depending on the facet selections, you may get a warning that 
                  too many plots will be generated to be useful (i.e. more than 20).  
                  Should this be the case, a button will be presented allowing you 
                  to override the warning and show all of the plots regardless. 
                  If you decide to view all of the plots, another button will replace 
                  the warning that will allow you to easilt 'de-facet your data.")
                 )
                 ,h4("I Need More Help!")
                 ,p("For assistance with this app, please contact ", a("Mike McMahon", href = "mailto:Mike.McMahon@dfo-mpo.gc.ca"))
                 ,p("This is my first Shiny app, so please be gentle.")
  )
  )
}