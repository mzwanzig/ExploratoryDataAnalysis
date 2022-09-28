# ELECTRONIC SUPPLEMENT TO A BOOK CHAPTER ON
# 'Primary Steps in Analyzing Data: Tasks and Tools for a Systematic Data Exploration'
# By: Zwanzig, M., Schlicht, R., Frischbier, N., Berger, U. (2020),
# In: Levia, D.F., Carlyle-Moses, D.E., Iida, S., Michalzik, B., Nanko, K., Tischer, A. (eds)
#     Forest-Water Interactions. Ecological Studies, vol 240. Springer, Cham.
# https://doi.org/10.1007/978-3-030-26086-6_7

# FEEL FREE TO USE AND MODIFY THE FOLLOWING CODE FOR YOUR OWN WORK.

# To get an overview, please show document outline (RStudio Shortcut: Ctrl + Shift + O)

# SETUP R ----

  # First, install all required packages (skip, if they are installed yet):
  install.packages("ggplot2")
  install.packages("RColorBrewer")
  install.packages("GGally")
  install.packages("OutliersO3")
  install.packages("knitr")
  install.packages("moments")
  install.packages("car")
  install.packages("plotly")
  # To use plotly on Linux systems you might have to install libssl-dev and libcurl4-openssl-dev first:
  # (and libxml2-dev is required for OutliersO3 or its XML-package dependency respectively)
  # (On ubuntu run the following commands in terminal:)
  # sudo apt-get update
  # sudo apt-get install libssl-dev
  # sudo apt-get install libcurl4-openssl-dev
  # sudo apt-get install libxml2-dev
  # All package installations worked on Ubuntu 18.04 with R version 3.5.1

  # Now load the packages:
  library(ggplot2)
  library(RColorBrewer)
  library(GGally)
  library(OutliersO3)
  library(knitr)
  library(moments)
  library(car)
  library(plotly)

  # Only necessary if you like to store and share your plotly figures online:
  # Set system environment to access your online plotly account for plot storage:
  source("setSysEnv.R") # directs to a local file providing the necessary information
  # (see https://plot.ly/r/getting-started/ for more informations)

  # set your working directory (path to the folder were your data is stored and will be saved to):
  setwd("path/to/your/data")

# IMPORT AND OVERVIEW ----

  # in the book chapter, the dataset is named the 'throughfall dataset', here shortly td
  td <- read.table("throughfall_dataset.txt") # to load the data stored in a txt file (in the working directory)
  head(td) # this is how the top of your data frame looks like (Any strange numbers/symbols?)

  # * Inspect and format raw data ----
  # !!! The subsequent reformating is always necessary after loading the raw data !!!
  
  # Which type do the variables have after loading the raw data and is this appropriate?
    
  str(td) #show the structure of the data (lists the type of each variable and first entries)
    # also lists the class of the object -> should be a data frame (columns seperate variables
    # of different type, e.g. categorical and continuous variables)
    
    # Definitions for the type of variables should be reconfigured here, e.g. checking that any continuous
    # variable is listed as 'num' (numeric) and any categorical variable as factor.
    td$funnel.id <- factor(td$funnel.id) # makes funnel.id a factor, since 'num' refers it to be a continuous variable
    td$tree.id <- factor(td$tree.id) # the same as for funnel.id
    
    # Time should be stored in a format that is directly readable by R commands (such as plot routines).
    # The class POSIXct represents time in YEAR-MONTH-DAY format (up to seconds is also possible,
    # but not given here). This allows to consider the temporal order and distance of events.
    td$obs.date <- as.POSIXct(td$obs.date)
    
    # Other fine adjustments are to force the levels of a factor to appear in an appropriate order,
    # e.g. in plots (an ordered factor is strictly speaking a type of scale of a variable, but here
    # it is appropriate to order a naturally unordered factor for some technical reasons):
    td$canopy <- ordered(td$canopy, c("beech","spruce","mixed","gap")) # desired order of canopy types

  # * Screen for missing values in the data ----

    # After this adjustements, lets make a summary statistics for each variable
    summary(td) # calculates basic parameters
    # Did you recognized unusual values? Are there NA's and is this feasible?
    # For example, throughfall.mm contains 1 NA. Thus, a value for the response variable is missing.
    which(is.na(td$throughfall.mm)) # Which is it?
    td <- td[-which(is.na(td$throughfall.mm)),] # lets throw this row out! '-' means erase row or column number ...
    # Please note that using the NA-detection command directly in the data frame
    # prevents that a row is deleted again, although all NA rows have been deleted yet
    # (this would happen if you just specify the number -884 and execute the line more than once)
    
  # * Screen for zeros in the data ----
    
    # count the number of observations with a certain number:
    table(td$throughfall.mm)
    # Create a frequency plot:
    barplot(table(td$throughfall.mm), col = "black", border = NA, space = 0.9,
            xlab = "Observed values", ylab = "Frequency")
    
  # * Count observations per group ----
    
    # Observations per period:
    table(td[["period"]]) # 'table' counts observations for each category of the specified variable
    # Observations for each canopy type in each period:
    with(td, table(period, canopy)) # 'with' applies this for two categorical variables (factors)
    # Observations for each measurement place (funnel) in each period:
    with(td, table(funnel.id, period))
    
    # How many funnels are associated with a single tree?
    # This is more complex to solve. First we need the number of funnels
    # per tree and precipiation event (obs.date):
    mm <- tapply(X = td$funnel.id, # tapply applies a function FUN over X (funnel.id)
                 INDEX = list(td$tree.id, td$obs.date),  # categorical variable(s) X is grouped to
                 FUN = length)  # the 'length' function counts the number of entries (= # funnels per tree and obs.date)
    # This can than be used to compute the range (min, max) over all obs.date for a tree
    apply(X = mm, MARGIN = 1, # means to apply the function over rows of 'mm'
          function(X){range(X, na.rm = TRUE)}) # the common 'range' function, but with NA removal
    # (-> column names represent tree.id's, first row = min, second row = max)
    # ...ok, but with which species are the tree.id's associated with?
    tapply(X = td$species, INDEX = list(td$tree.id), FUN = unique)
    # ok, gives class 1 or 2 for every tree id, but what means 1 and 2? ...
    levels(td$species) # ...shows that 1 = 'beech' and 2 = 'spruce'
    
  # * Locate measurement points ----
    
  # Where at the study area are the measurement points (i.e. funnels)?
    
    # Specifying a general color scheme for the categorical variable canopy type (will be re-used multiple times)
    canopy.colors <- c(brewer.pal(12, "Set3")[c(4,1,6,5)]) # extracts some colors from the brewer palette 'Set3'
    
    # Mapping data on funnels:
    p <- plot_ly(data = td[!duplicated(td$funnel.id),], # ignoring duplicate information for each funnel
                 x = ~funnel.X.m, y = ~funnel.Y.m, # points should be drawn at these funnel coordinates
                 type = 'scatter', mode = 'markers',
                 color = ~canopy, colors = canopy.colors, # color points according to canopy type
                 hoverinfo = 'text', # the following information should be interactively available for each point:
                 text = ~paste('Funnel.ID:', funnel.id,
                               '<br>Canopy:', canopy,
                               '<br>Canopy tree ID:', tree.id,
                               '<br>abs. distance [m]:', round(abs.distance,2),
                               '<br>rel. distance [-]:', round(rel.distance,2)),
                 width = 500, height = 400)
    p # plot locally ...
    chart_link = api_create(p, filename="throughfall-data-map") # ...or store and view online (accessible via link)
    # using your plotly account you can also modify and/or download your figure
    # for interactive offline usage as a html file
    
# INDEPENDENCE OF OBSERVATIONS ----
  
  # The assumption of independence among observations is crucial for most statistical tests.
  # First of all independence has to be reasonable according to the design of the experiment.
  # Anyway, independence should be tested before doing the analysis as well as for the
  # residuals afterwards. Basic methods are to plot the response variable vs. time and
  # space. Any clear pattern is a sign of dependence.
  # ... Note that some models can work well, when (detected) dependence structures are defined!
    
  # * Inspect the response vs. space ----
    
  # ** ... for single time points ----
    
    # This can be achieved by plotting the amount of throughfall that was measured at any funnel
    # for each single precipiation event:
    ggplot(data = td, # the data frame that will be used to extract data for plotting
           aes(x = funnel.X.m, y = funnel.Y.m, # define aesthetics, here plot points according to the funnel coordinates
               col = throughfall.mm)) + # color them by the amount of throughfall
      geom_point(aes(shape = canopy)) + # vary their shape according to the funnnel's canopy type
      coord_fixed(ratio = 1) + # fixes the aspect ratio of both axes to 1 (length units are equivalent)
      facet_wrap(.~ as.factor(obs.date)) # create a subplot for each precipiation event
    # -> the result is not clear, since the color contrast is only weak according to the dominance
    #    of a single event with a high amount of throughfall.
    # Optionally plot transformed throughfall data, e.g. using a cube transformation (for this data)
    # or a logarithmic transformation (might be more appropriate for other data; see next sections for more details)
    # We skip the transformation and instead express throughfall as a fraction of gross precipitation:
    ggplot(data = td, aes(x = funnel.X.m, y = funnel.Y.m,
                           col = throughfall.mm / gross.prec.mm)) + # we can also apply calculations here!
      geom_point(aes(shape = canopy), size = 0.5) +
      coord_fixed(ratio = 1) +
      facet_wrap(.~ as.factor(obs.date)) + # it is also interesting to order events by gross.prec.mm!
      xlab("X coordinate [m]") + ylab("Y coordinate [m]") +
      labs(color = "Throughfall [-]", shape = "Canopy")
    # -> the color contrast is enhanced! We can inspect the plots for spatial patterns:
    # There is a great variety between precipitation events, but most subplots appear
    # rather homogeneous in their color within a precipiation event. The only visual gradient
    # within a precipiation event is given between points belonging to different canopy classes.
    # For instance funnels in gap position (marked with +) always have a higher amount of throughfall.
    # The source of the visible variation is explainable by nesting and remarkable gradients for points
    # beloning to the same canopy type cannot be identified. Considering this, it can be concluded:
    # -> No spatial patterns -> no spatial dependence structure -> Independence among observations!
    ggsave("response.vs.space.pdf", width = 7, height = 7) # saves the last plot acc. to the given path/name and size (inch)
    
  # ** ... aggregating time points ----
  
    # As mentioned in the main text of our book chapter, it is desirable to aggregate data,
    # e.g. creating a single map showing estimates for each funnel based on all observation dates
    # of a single period (we still have to account for the grouping structure!).
    # Here, we have to consider that the data is unbalanced.
    # (some precipiation events were not recorded at some funnels)
    
    # Lets focus on the leafed period data:
    td.l <- subset(td, td$period == "leafed") # creates a subset containing only the leafed period data
    
    # *** Step 1: Identify those dates when entries for all measurement points are present (when data is unbalanced)
    max.obs <- length(unique(td.l$funnel.id)) # creates a list of unique names and counts its length (the number of different measurement points)
    obs.list <- tapply(td.l$funnel.id, INDEX = list(td.l$obs.date), FUN = length) # creates a list with the number of different funnel.ids per obs.data
    des.dates <- names(obs.list)[obs.list == max.obs] # extracts those obs.dates that hold this condition (our desired dates)
    
    # *** Step 2: Create a subset of data containing only the desired dates
    # 'as.POSIXct' means that des.dates is converted to the same format as obs.date
    td.sub <- subset(td.l, td.l$obs.date %in% as.POSIXct(des.dates)) # performs an intersection
    
    # *** Step 3: Check the consistency of the resulting data subset. Are NA's present? 
    sum(is.na(td.sub$throughfall.mm)) # count NA's
    NA.dates <- td.sub$obs.date[which(is.na(td.sub$throughfall.mm))] # Which dates are these NA's aligned with?
    # if present, throw out the NA dates, leaving a clean subset
    if (length(NA.dates) > 0) {td.sub <- subset(td.sub, td.sub$obs.date != NA.dates)}
    
    # *** Step 4: Create a new dataframe containing various calculated throughfall estimates for each funnel
    my.data <- td.sub # create a copy to work with or define your data here (e.g. the data in random order (line 228-229))
    md <- data.frame(
      throughfall.sum = tapply(my.data$throughfall.mm, INDEX = list(my.data$funnel.id), FUN = sum), # calculates sums (you may also use 'mean' or 'median' here)
      throughfall.prop.median = tapply(my.data$throughfall.mm / my.data$gross.prec.mm, # using the throughfall porportion ...
                                   INDEX = list(my.data$funnel.id), FUN = median), # ... and calculating its median
      X.coord = tapply(my.data$funnel.X.m, INDEX = list(my.data$funnel.id), FUN = mean), # extracts respective X coordinates
      Y.coord = tapply(my.data$funnel.Y.m, INDEX = list(my.data$funnel.id), FUN = mean), # extracts respective Y coordinates
      canopy = tapply(as.numeric(my.data$canopy), INDEX = list(my.data$funnel.id), FUN = mean) # extracts respective canopy information
    )
    str(md) # inspect the structure of the data frame
    
    # *** Step 5: Create and inspect spatial plots for dependence structures
    # Is the resulting pattern somehow random or does it show some clustering?
    ggplot(data = md, aes(x = X.coord, y = Y.coord,
                          col = throughfall.sum)) + # color points according to the cumulative values of the response variable
      geom_point(size = 3, # increased point size enhances visibility
                 aes(shape = factor(canopy, labels = c("beech", "spruce", "mixed", "gap")))) + # point shape reflects canopy
      coord_fixed(ratio = 1) + # fixed / equal aspect ratio (length in x direction is the same as in y direction)
      labs(color = "Cum. throughfall [mm]", shape = "Canopy") # adjusting the titles given by the factors used for the color and shape attributes
    # -> no clear pattern! -> supports previous reasoning
    # and now using the median for the throughfall proportion
    ggplot(data = md, aes(x = X.coord, y = Y.coord, col = throughfall.prop.median)) +
      geom_point(size = 1, aes(shape = factor(canopy, labels = c("beech", "spruce", "mixed", "gap")))) +
      coord_fixed(ratio = 1) + xlab("X coordinate [m]") + ylab("Y coordinate [m]") + # here also renaming the axis labels
      labs(color = "Median(Throughfall [-])", shape = "Canopy")
    # -> no clear pattern! -> supports previous reasoning
    ggsave("response.vs.space_agg.pdf", width = 4, height = 4) # saves the last plot acc. to the given path/name and size (inch)

    # Unsure if there is a pattern or not? Reorder your data randomly ...
    td.rt <- td.sub # create a copy
    random.order <- sample(1:nrow(td.sub)) # sampling the current order without replacement creates a random order
    td.rt$throughfall.mm <- td.sub$throughfall.mm[random.order] # gives throughfall the specified random order
    td.rt$gross.prec.mm <- td.sub$gross.prec.mm[random.order] # and the same to gross prec. (-> pairs remain the same!)
    # ... and plot it again after replacing the data frame 'td' used for ggplot in line 188 by 'td.rt'.
    # Repeat this multiple times and compare the random patterns to your data.
    # (better results are obtained when groups (e.g. canopy) are considered by reorder (sample only the same group))
        
  # * Inspect the response vs. time ----
  
    # The data can and should be inspected in various ways and at different levels for this aspect.
    # First, the raw data should be illustrated, e.g. plotting a time series for each funnel:
    ggplot(data = td, aes(x = obs.date, y = throughfall.mm / gross.prec.mm, col = canopy)) +
      geom_line() + # the 'line' geom draws lines instead of points now
      facet_wrap(. ~ funnel.id) + # creates single plots (and therefore also lines) for each funnel
      scale_color_manual(values = canopy.colors) # our desired color scheme, defined in line 72 (skip for auto colors)
  
    # Second, we like to check autocorrelation.
    
      # *** Step 1: Lets create a table in width format with columns representing single time series:
      funnel.ts <- tapply(td$throughfall.mm / td$gross.prec.mm, # the response: the proportion of throughfall
                          list(td$obs.date, td$funnel.id), # grouped according to time and funnel
                          function(x){x}) # this function only returns the value itself (data is only reordered with this tapply function!)
      random.ts <- funnel.ts; random.ts[] <- rnorm(nrow(funnel.ts) * ncol(funnel.ts)) # create random data
      ts.df <- funnel.ts # either specify the real data 'funnel.ts' or the random data 'random.ts' here
      
      sum(is.na(ts.df)) # ok, it contains a lot of NA, since some funnel did not recorded some precipitation events
      na.f <- apply(ts.df, 2, function(x){sum(is.na(x))}); na.f # which contain NA's?
      
      # *** Step 2: Lets calculate the correlation between all time series of those funnels that registered all events:
      ts.cor <- cor(ts.df[,na.f == 0])
      hist(ts.cor) # funnel specific time series are all highly correlated!
      
      # *** Step 3: Lets inspect a single time series more in detail (we know now that they are all quite similar)
      layout(matrix(1:2)) # make space for two subfigures
      par(mar = c(4,4,3,1)) # redefining subfigure margins
      ts.no <- 5 # a variable that will refer to the ... column of 'ts.df' (5 = the data of funnel.id 105)
      plot(x = as.POSIXct(rownames(ts.df)), y = ts.df[,ts.no],
           type = "b", pch = 16, col = grey(0, 1/2), main = paste("Funnel ID:",levels(td$funnel.id)[ts.no]),
           xlab = "Observation date of precipiation event", ylab = "Throughfall [-]")
      # And now estimate autocorrelation, which is usally intended for equally spaced samples (e.g. every day)
      # We apply ACF here assuming that observations are equal according to the sequence of events (one event follows the other)
      # (see the chapter text for more details):
      acf(ts(ts.df[,ts.no]), main = "", xlab = "Lag (precipiation event)")
      title(main = paste("Funnel ID:",levels(td$funnel.id)[ts.no]), line = 1)
      
      dev.off() # return adjustments of the graphic device
    
      # *** Step 4: Lets apply acf for more of our time series to see if this result can be generalized:
      # the default avf method can be applied to multiple time series,
      # but creates a huge matrix of acf plots, when there are many time series
      # Lets make space for such a figure by creating a large pdf
      pdf("multiple_time_series_acf.pdf", width = 20, height = 20)
      acf(ts(ts.df[,na.f == 0]), lag.max = 15) # run it excluding all incomplete time series
      dev.off() # close the device to finalize the pdf generation
      
      # We can also aggregate this information combining acf results of single time series in boxplots
      par(mar = c(4,4,1,1)) # again, set appropriate figure margins widths
      acr <- NULL # lets create an empty object named acr
      for(i in which(na.f == 0)){ # the index i should include all complete funnel time series
        acr <- cbind(acr, acf(ts(ts.df[,i]))$acf) # apply acf and extract only the correlation 'acf'
      } # close 'for'-loop; this loop makes acr a matrix with columns representing the data of each funnel 
      # Lag-specific correlations for all funnels can be represented in a boxplot:
      boxplot(t(acr), names = 0:15, main = "Summary of 118 funnel-specific time series",
              xlab = "Lag (precipiation events)", ylab = "Funnel specific ACF")
      abline(h = 0, col = grey(0,0.5)) # draw a solid, slightly transparent '0 line'
      CI95 <- qnorm(0.95) / sqrt(nrow(acr)) # the confidence interval can be computed from a normal distribution
      abline(h = c(CI95, -CI95), lty = 2, col = "blue") # 95% confidence intervall; based on uncorrelated series
      # -> A summary of time series specific autocorrelations
      # -> Indicates no concerning degree of autocorrelation!
      
      # Not sure about patterns? Compare your data to purely random data (see lines 249-250)
    
# OUTLIERS ----
  
  # Identify potential outlier for all response and explanatory variables, considering
  # one or more dimensions. Please note that on potential outliers should first of all
  # be kept an eye on, but they don't have to be necessarily removed. To evaluate this,
  # it is necessary to test their influence on the results of the model that you like
  # like to apply (either testing datasets with and without the outlier(s) or using model
  # diagnosis tools, as e.g. 'Cook's distance plots' given for linear regression).
      
  # * Draw boxplots ----
    # ** Simple boxplots
    boxplot(td$throughfall.mm) # for total throughfall in mm
    boxplot(td$throughfall.mm / td$gross.prec.mm) # for throughfall as a proportion of gross precipiation
    
    # ** Conditional boxplots
    # adding canopy -> two dimensions:
    boxplot(td$throughfall.mm / td$gross.prec.mm ~ td$canopy) 
    # attention, after executing the following line of code,
    # click on points in the figure, then press ESC
    identify(y = td$throughfall.mm / td$gross.prec.mm, x = td$canopy)
    # adding period -> three dimensions:
    boxplot(td$throughfall.mm / td$gross.prec.mm ~ td$canopy + td$period)
    # 'identify' is incompatible to the nested grouping
    # The figure could look much better for the grouping ... use ggplot!
    
    # ** Using ggplot
    # (a more attractive form of data visualization with nice grouping features)
    p <- ggplot(data = td, aes(y = throughfall.mm / gross.prec.mm)) # defining the basic plot routine, we name the object "p"
    p + geom_boxplot() # adding a component ("layer") to 'p'; gives a simple boxplot of throughfall [-]
    p + geom_boxplot(aes(x = canopy)) # adding a more advanced layer to p by defining x as a grouping factor (for canopy types)
    # alternatively, we can define grouping by more than one factor ...
    bp <- p + geom_boxplot( # (the new object should be named 'bp' and is build by 'p + ...')
              aes(fill = period), # to differentiate boxplots of other period by color
              outlier.alpha = 1/3, # (transparency for outlier points)
              varwidth = TRUE) + # (width of the boxes should be relative to number of observations)
      scale_fill_brewer(type = "qual") +
      facet_grid(~ canopy + period) + # ...using 'facet_grid' for a defintion of 'row factor ~ column factor'
      ylab("Throughfall [-]") + # redefining the y axis label
      theme_minimal() +
      theme(axis.text.x = element_blank(), # eleminates x axis text ...
            axis.ticks.x = element_blank()) # ... and ticks, which are obiously unnecessary
    bp
    ggsave("ConditionalBoxplot.pdf", width = 5, height = 3) # save figure as name.filetype with sizes in inch
    
    # ** Using plotly
    # (poviding interactivity as a feature)
    ggplotly(p+ geom_boxplot()) # for the simple boxplot
      
    ggplotly(bp) # unfortunately, this doesn't work here, although the web shows many examples for similar functions
    # lets build a plot by some plotly functions:
    td$obs.id <- 1:nrow(td) # adding an observation identity number to the data
    pl <- plot_ly(data = subset(td, td$period == "leafed"), # defining the dataframes name or creating a subset 
                  x = ~ canopy, # defining x (note that '~' is used here)
                  y = ~ throughfall.mm / gross.prec.mm, # the same for y
                  type = 'box', mode = 'markers',
                  hoverinfo = 'text', # the following information should be interactively available for each point:
                  text = ~paste('Obs.ID:', obs.id,
                                '<br>Throughfall [mm]:', throughfall.mm,
                                '<br>Gross prec. [mm]:', gross.prec.mm,
                                '<br>Obs. date:', obs.date,
                                '<br>Period:', period,
                                '<br>Funnel ID:', funnel.id,
                                '<br>Canopy:', canopy,
                                '<br>Abs. distance:', abs.distance,
                                '<br>Rel. distance:', rel.distance,
                                '<br>Tree ID:', tree.id,
                                '<br>Tree height:', tree.height.m,
                                '<br>Tree stem diameter:', tree.dbh.cm,
                                '<br>Crown radius:', crown.radius.m,
                                '<br>Crown beginning:', crown.beginning.m))
    pl2 <- pl %>% layout(title = "Leafed period", # some more layout options can be defined here
                         yaxis = list(title = "Throughfall [-]")) 
    pl2 # creates the interactive figure (note that it appears in the "viewer" or your browser, since it is html based)
    # -> try the zoom function to identify plots that might overlay each other!
    chart_link = api_create(pl2, filename="conditional-boxplot") # store and view online (accessible via link)
    
    # Using the obs.id you can also extract every information about an outlying observation point from the data frame
    td[2417,] # moving the mouse to the outlier top left, we see it has id 2417; this index gives you all infos from this row
    
  # * Draw Cleveland dotplots ----
      
    # ** Simple dotcharts
    # (data is plotted in the order given in the data table)
    # Lets apply a defualt plotting routine for all columns of our data table:
    names(td) # shows variable names ... ok we have 18 variables
    par(mar = c(2,2,2,1)) # adjust figure margins
    layout(matrix(1:18, 3, 6)) # make space for 18 plots
    for (i in 1:18){ # repeat 18 times; plot each time another column of the data frame 'td'
      dotchart(x = as.numeric(td[,i]), main = names(td)[i]) # title = column name; transparent, filled points
    }
    
    dev.off() # restores default layout of graphic panel
    
    # ** Conditional dotcharts
    dotchart(x = td$throughfall.mm/td$gross.prec.mm, main = "Throughfall [-]",
             groups = td$canopy) # distinguishing by canopy
    dotchart(x = td$throughfall.mm/td$gross.prec.mm, main = "Throughfall [-]",
             groups = td$canopy, col = td$period) # distinguishing by canopy and period
        
    # ** Using ggplot
    ggplot(data = td, aes(x = throughfall.mm / gross.prec.mm, y = 1:dim(td)[1])) +
      geom_point(alpha = 1/10, stroke = NA) +
      xlab("Throughfall [-]") + ylab("Order of the data")
    ggplot(data = td, aes(x = throughfall.mm, y = 1:dim(td)[1])) +
      geom_point(alpha = 1/10, stroke = NA) +
      xlab("Throughfall [mm]") + ylab("Order of the data")
    ggplot(data = td, aes(x = gross.prec.mm, y = 1:dim(td)[1])) +
      geom_point(alpha = 1/10, stroke = NA) +
      xlab("Gross precipitation [mm]") + ylab("Order of the data")
    ggplot(data = td, aes(x = abs.distance, y = 1:dim(td)[1])) +
      geom_point(alpha = 1/10, stroke = NA) +
      xlab("Abs. distance [m]") + ylab("Order of the data")
    ggplot(data = td, aes(x = rel.distance, y = 1:dim(td)[1])) +
      geom_point(alpha = 1/10, stroke = NA) +
      xlab("Rel. distance [-]") + ylab("Order of the data")
    
    # ** Using plotly
    # convert any of the previous ggplots using 'ggplotly', e.g.:
    p <- ggplot(data = td, aes(x = throughfall.mm / gross.prec.mm, y = 1:dim(td)[1])) +
      geom_point(alpha = 1/10, stroke = NA) +
      xlab("Throughfall [-]") + ylab("Order of the data")
    ggplotly(p)
    
  # * Draw scatterplots ----
      
    # Plotting variable vs. variable shows if some observations 
    # deviate from the regular pattern. This should in principle be
    # examined for every variable combination. Here we create a pdf with
    # a single page for every scatterplot. This maximizes the resolution,
    # but does not provide a fast overview.
    names(td) # shows variable names ... ok we have 18 variables
    pdf("scatterplot_sequence.pdf", width = 7, height = 7) # stores all plots succesively in a pdf
    for (j in 1:18){
      for (i in 1:18){ # repeat 18 times; plot each time another column of the data frame 'td'
        if(i > j){ # prevents to plot combinations twice
          plot(x = as.numeric(td[,i]), y = as.numeric(td[,j]), xlab = names(td)[i],
               ylab = names(td)[j], pch = 16, col = grey(0, alpha = 1/10)) # title = column name; transparent, filled points
        }
      }
    }
    dev.off() # closing the opened 'pdf' device will finalize the figure
    # open the pdf now in your working directory and check single plots 
    # to identify suspicious variable interaction
    
    # another method is to create a single figure that shows all scatterplots directly
    # ('ggpairs' presented in 'RELATIONSHIPS OF VARIABLES' represents a more powerful
    # alternative to 'pairs', but might not work on some machines, so ...)
    pdf("scatterplot_matrix.pdf", width = 20, height = 20) # enough space for all plots
    pairs(td[,1:18]) # computation can take time or is aborted, when figure size is too small (no pdf)
    dev.off() #finalizes pdf; can be opened now with external viewer
    
    # *** Using ggplot
    ps <- ggplot(data = td, aes(x = rel.distance, y = throughfall.mm / gross.prec.mm)) +
      geom_point()
    ps # show it
    
    # *** Using plotly
    # convert any of the previous ggplots using 'ggplotly', e.g.:
    td$obs.id <- 1:nrow(td) # adding an observation identity number to the data (if not yet applied)
    ps <- ggplot(data = td, aes(x = rel.distance, y = throughfall.mm / gross.prec.mm,
                                 col = obs.id, shape = canopy)) + # obs.id is somehow inclduded in the ggplot, although it makes no sense to color it
      geom_point() +
      xlab("Relative distance [-]") + ylab("Throughfall [-]") 
    ggplotly(ps, tooltip = c("x", "y", "obs.id")) # here, all variables incl. obs.id can be text-highlighted
  
    # this can be done for nearly any variable combination
    ggplot(data = td, aes(x = tree.height.m, y = tree.dbh.cm,
                                 col = species, shape = species)) + # obs.id is somehow inclduded in the ggplot, although it makes no sense to color it
      geom_point() +
      xlab("Tree height [m]") + ylab("Tree diameter at breast height [cm]") +
      scale_color_manual(values = canopy.colors[1:2]) # color set for canopy types as defined in line 113

    ggplot(data = td, aes(x = crown.radius.m, y = crown.beginning.m,
                                 col = species, shape = species)) + # obs.id is somehow inclduded in the ggplot, although it makes no sense to color it
      geom_point() +
      xlab("Crown radius [m]") + ylab("Crown beginning [m]") +
      scale_color_manual(values = canopy.colors[1:2]) # color set for canopy types as defined in line 113
    
  # * Perform formal tests ----
    
    # The OutliersO3 package provides a routine for outlier detection applying manifold
    # formal tests simultaneously
    
    vignette("O3-vignette") # an introduction to OutliersO3 to get some more detailed examples and further help
    
    # First, prepare a data frame
    # We have to predefine the data in use (the method itself does not account for the nested data structure)
    names(td) # ...to remember the names of variables and their column position
    td$obs.id <- 1:nrow(td) # adding an observation identity number to the data (if not yet applied)
    td$throughfall.prop <- td$throughfall.mm / td$gross.prec.mm # define throughfall proportion (as new variable)
    td.sub <- subset(td, td$period == "leafed" & td$canopy == "beech") # use only a subset first (all data can be used too)
    my.sub1 <- td.sub[,c(20, 10, 17)] # extract only those variables that should be checked for outliers (we ignore some irrelevant here)
    rownames(my.sub1) <- td.sub$obs.id # give a real id to the observations
    # detect where NA's are -> outlier detection does not work with NA's
    del1 <- which(is.na(my.sub1[,1])) # in first column
    del2 <- which(is.na(my.sub1[,2])) # in second column
    del3 <- which(is.na(my.sub1[,3])) # in third column
    NA.positions <- c(del1,del2,del3) # merge relevant NA observations of all columns
    if (length(NA.positions) > 0){ # if any NA's were found ...
      my.sub <- my.sub1[-NA.positions,] # ...create a new NA-free subset of the data
    }else{ # if no NA's were found ...
      my.sub <- my.sub1 # ... just copy the whole data frame
    }
    which(is.na(my.sub)) # successful? (= integer(0)) ...otherwise repeat NA detection
    
    Os <- O3prep(data = my.sub) # the data is prepared
    Os1 <- O3plotT(Os) # outlier detection is processed
    Os1$nOut # show number of outliers found
    Os1$gpcp # show parallel coordinate plot with outlier cases in red
    # -> many outliers refer to huge crown radius!
    Os1$gO3 # plot the results in a table-like figure (increase it (by printing a pdf))
    # -> ok, we see there are some outliers in one- and two-dimensional space
    # Note that the grey boxes on the left indicate the combination of variables for
    # which the outliers to the right have been detected (in red, and named with ID by column)
    # If a variable (-combination) is missing, then no outliers were found for it
    # -> no outliers are found for relative distance, relative distance vs. crown radius
    # and crown radius vs. throughfall proportion!

    # comparing different outlier tolerance levels:
    Ot <- O3prep(data = my.sub, tols = c(0.05, 0.025, 0.01), boxplotLimits = c(3, 6, 10))
    Ot1 <- O3plotT(Ot)
    Ot1$nOut # show number of outliers found per tolerance level
    # make a combined plot for parallel coordinate plot and table-like overview
    gridExtra::grid.arrange(Ot1$gO3, Ot1$gpcp, ncol=1) # refers to R package 'gridExtra'
    # -> we see that the identified outliers have different tolerance levels (...are more or less severe)
    
    # comparing different outlier detection methods:
    Om <- O3prep(data = my.sub, # apply only for throughfall.prop and rel.distance here
                  method= c("HDo", "PCS", "BAC", "adjOut", "DDC", "MCD"), # use all available methods
                  tols=0.05,  boxplotLimits=c(3))
    Om1 <- O3plotM(Om)
    Om1$nOut # show number of outliers found per method
    gridExtra::grid.arrange(Om1$gO3, Om1$gpcp, ncol=1)
    # -> only two observations where identified by two methods, various by one, none by all!
    
  # * Identify influential observations ----
    
    # to do so, we require a statistical analysis in use, since the influence is
    # measured regarding the effect of an observation on the test results
    
    # Lets generate simple regresssion model
    lm1 <- lm(throughfall.mm ~ gross.prec.mm * canopy * period, data = td)
    # -> throughfall in dependence to gross precipitation and canopy type
    # ...we ignore other details of the model and do only create the Cook's
    # distance plot:
    plot(lm1, which = 4)
    # -> three observations are labelled, since they have a rather large influence
    # on the model results
    # If we change the structure, e.g. removing the three-way interaction,
    # the Cook's distance also change:
    lm2 <- lm(throughfall.mm ~ gross.prec.mm + canopy * period, data = td)
    plot(lm2, which = 4)
    # -> only one of the previous influential observations is labelled again
    # -> in general, these type of outliers are more severe than the potential
    # outliers detected by graphical routines
    # Lets apply the model without these observations and compare both results:
    str(lm2) # thats the structure of the lm object resulting from the regression
    # names of the residuals are the labelled observations. In which row of the data are they?
    IO <- which(names(lm2$residuals) %in% c("719", "6724", "9323")) # get row number of influential observations (IO)
    lm2r <- lm(throughfall.mm ~ gross.prec.mm + canopy * period, data = td[-IO,]) # remove IO
    plot(lm2r, which = 4) # ...again, some observations are labelled, but they have less deviation to the other observations than before
    # both models can be compared in detail using the complete summary statistics on lm ...
    summary(lm2) # containing all observations
    summary(lm2r) # with removed influential observations
    # ... or just focussing in the effect on parameter estimates:
    barplot(lm2$coefficients - lm2r$coefficients, las = 2)
    # ... we will stop here, although model diagnosis and modification could be continued
    # see "Regression example: gross precipiation vs. throughfall" for further details
    
# NORMALITY AND HOMOSCEDASTICITY ----
  
  # * Inspect the distribution of the response ----
    
    # ** Draw histograms
    
    hist(td$throughfall.mm) # for total throughfall in mm
    # -> right-skewed distribution
    hist(td$throughfall.mm / td$gross.prec.mm) # for throughfall as a proportion of gross precipiation
    # -> left-skewed distribution

    # ** Draw density plots
    
    plot(density(td$throughfall.mm)) # for total throughfall in mm
    plot(density(td$throughfall.mm / td$gross.prec.mm)) # for throughfall as a proportion of gross precipiation
    
    # ** Draw QQ-plots
    
    qqnorm(td$throughfall.mm) # for total throughfall in mm
    qqnorm(td$throughfall.mm / td$gross.prec.mm) # for throughfall as a proportion of gross precipiation
    qqline(td$throughfall.mm / td$gross.prec.mm) # line that connects the first and third quartile of the theoretical distribution
    # Do the points follow the straight line? If not, this indicates deviations from the normal distribution
    
    # ** Using ggplot
    
    p <- ggplot(data = td, aes(throughfall.mm)) # basic
    p + geom_histogram() # basic + histogram
    p + geom_density(fill = "black") + xlab("Throughfall [mm]") + ylab("Density") # basic + density plot
    ggsave("response_densityplot.pdf", width = 2, height = 2)
    ggplot(data = td, aes(sample = throughfall.mm)) + # new basic definition
      geom_qq(size = 1/5) + # ...for drawing qqplot (reduced point size)
      geom_qq_line(size = 1/5) # theoretical straight line the points should follow (reduced line width)
    ggsave("response_vs_normal.pdf", width = 2, height = 2)
    
    # ** Using formal tests
    
    # for throughfall data in mm:
    shapiro.test(td$throughfall.mm) # our sample is too large for the shapiro test
    ks.test(x = td$throughfall.mm, y = "pnorm") # -> p-value < 0.05 -> not normally distributed
    agostino.test(td$throughfall.mm) # from 'moments' package
    # -> positive skewness -> p-value < 0.05 -> null-hypothesis rejected -> data is skewed
    # for throughfall as a proportion of gross precipitation:
    ks.test(x = td$throughfall.mm / td$gross.prec.mm, y = "pnorm") # -> p-value < 0.05 -> not normally distributed
    agostino.test(td$throughfall.mm / td$gross.prec.mm)
    # -> p-value < 0.05 -> null-hypothesis rejected -> data is skewed
    
  # * Inspect the group-specific spread of the response ----
    
    # ** Draw conditional boxplots
    boxplot(td$throughfall.mm ~ td$period) # for total throughfall in mm
    boxplot(td$throughfall.mm / td$gross.prec.mm ~ td$period) # for throughfall as a proportion of gross precipiation
    boxplot(td$throughfall.mm / td$gross.prec.mm ~ td$canopy + td$period) # for throughfall as a proportion of gross precipiation
    
    # ...and many more possibilities: see 'Draw boxplots' in 'OUTLIERS' section
    # Variance homogeneity has to be checked for the residuals of a model
    # Lets generate an exemplary, very simple model 'lm1':
    lm1 <- lm(throughfall.mm ~ gross.prec.mm * canopy * period, data = td)
    # -> throughfall in dependence to gross precipitation and canopy type
    # ...we ignore other details of the model and simply have a look 
    # on the variance of the residual per canopy type:
    plot(lm1$residuals ~ lm1$model$canopy)
    # -> variances are not that different between canopy types, but this is not
    #    really informative, since the model actually assumes that variances are also
    #    equal at any value of gross precipitation.
    # Using ggplot we can create a multi-panel scatterplot showing the
    # residual variance at any value of gross precipitation for each canopy type:
    # Lets use 'qplot' here, the quick plot function of ggplot.
    # ...'quick' is relative here, since we have to create a dataframe first in
    # order to apply facetting
    my.df <- data.frame(residuals = lm1$residuals,
                        gross.prec.mm = lm1$model$gross.prec.mm,
                        canopy = lm1$model$canopy,
                        period = lm1$model$period)
    qplot(data = my.df, y = residuals, x = gross.prec.mm, facets = canopy ~ period) # xy-plot
    # -> residual variance increases with gross precipitation! (the linear model 
    #    cannot be used in this way!)
    # Using lattice we can create the same more easily (without generating a dataframe)
    library(lattice)
    xyplot(lm1$residuals ~ lm1$model$gross.prec.mm | lm1$model$canopy + lm1$model$period)
  
# RELATIONSHIPS OF VARIABLES ----
  
  # * Inspect relations and interactions of variables ----
    
    # Inspect all pairs of variables:
    # A generalied pairs plot can be 'automatically' generated
    names(td) # resume the column order of given variables (used for subset selection)
    pm <- ggpairs(data = td, # the data frame containing all variables
                  columns = c(1, 2, 9, 10, 15:18, 4, 14, 8), # variables to explore by building (scatterplot) pairs
                  aes(colour = canopy, alpha = 0.5)) # colour them according to canopy type
    pm # plot it as usual
    pmi <- ggplotly(pm) # or transform the ggplot-object to an interactive plotly object
    chart_link = api_create(pmi, filename="throughfall-data-pairs-plot") # ...or store and view online (accessible via link)
  
    # Create Scatterplots
    # ... relating the response to the explanatory variables
    # See 'Regression example: gross precipiation vs. throughfall' for the application of
    # a multi-panel scatterplot in conjunction with a linear model for each panel.
    # Such scatterplots can be used to show how interactions affect for instance
    # the slope of a linear model. (see discussion in main text)
    
    # Create Coplots
    
    # lets modify our coplot using a function to create regression lines:
    panel.fun <- function(x,y,...){
      panel.lm <- lm(y ~ x, na.action = na.omit)
      points(x,y)
      abline(panel.lm, col = "blue", lwd = 2)
    }
    
    coplot(throughfall.mm  ~ gross.prec.mm | rel.distance * species,
           data = subset(td, td$period == "leafed" & is.na(td$species) == FALSE), # use only data from Leafde period and canopy =spruce or =beech
           panel = panel.fun) # using the self-defined panel function
    # -> ideally balanced design for relative distance in each species
    
    #  focussing on relative throughfall and accounting for nested structure by
    # precipiation event (given by gross prec. height) and species
    coplot(throughfall.mm / gross.prec.mm  ~ rel.distance | gross.prec.mm * species,
           data = subset(td, td$period == "leafed" & is.na(td$species) == FALSE), # use only data from Leafde period and canopy =spruce or =beech
           number = 10,
           panel = panel.fun) # using the self-defined panel function
    # -> again, design seems to be sufficiantly balanced to investigate this relationship
    
    # ... now according to tree height
    coplot(throughfall.mm / gross.prec.mm  ~ tree.height.m | gross.prec.mm * species,
           data = subset(td, td$period == "leafed" & is.na(td$species) == FALSE), # use only data from Leafde period and canopy =spruce or =beech
           number = 10,
           panel = panel.fun) # using the self-defined panel function
    # -> indicates an opposite relationship of proportional throughfall to tree height
    #    for both species; most ponounced differences at intermediate precipiation intensity

    # Create design plots
    # .. used to study interactions including many categorical variables:
    plot.design(throughfall.mm / gross.prec.mm ~ canopy + period, data = td) # interaction to relative throughfall
    plot.design(crown.radius.m ~ canopy + period, # interaction to crown.radius
                data = subset(td, td$canopy != "gap" & td$canopy != "mixed"))
    
    # Create interaction plots
    interaction.plot(x.factor = td$canopy,
                     trace.factor = td$period,
                     response = td$throughfall.mm / td$gross.prec.mm,
                     xlab = "Canopy type", ylab = "Throughfall [-]", trace.label = "Period")
    # -> Do the lines cross? Than an interaction between both factors is given.
    #    Parallel lines indicate no interaction, as given here except for the
    #    mean values in gap position were period has an oposite effect than for
    #    beech, spruce and mixed canopy
    
  # * Inspect collinearity ----
    
    # Computing correlation coefficients
    td.sub <- subset(td, is.na(td$species) == FALSE)[,c(2,9,10,15,16,17,18)]
    cor.mat <- cor(td.sub) # use only observations related to trees (to account for all tree variables)
    cor.mat > 0.7
    # -> critical are abs.distance-rel.distance and tree.dbh.cm-tree.height.m
    
    # Creating a scatterplot matrix (as before) including corr. coefficients
    pairs(td.sub, # use the same subset as before
          lower.panel = function(x,y,...){ # compute and print correlation coefficient
            xy.cor <- round(cor(x,y), 2) # calculate Pearson r and round it to 2 digits
            cor.col <- c("grey", "red")[1 + as.numeric(xy.cor > 0.7)] # determine conditional color
            text(x = min(x) + (max(x)-min(x)) / 2, # Position in the middle of x ...
                 y = min(y) + (max(y)-min(y)) / 2, #... and y
                 labels = xy.cor, #correlation coefficient
                 col = cor.col)}) #conditional color
    # Note that correlation coefficients can be increased considering
    # the grouping structure (see generalized pairs plot in line 658)
    
    # generating a scatterplot matrix using a data frame containing the subset
    # continuous explanatory variables for funnels directly associated to a single tree:
    ggpairs(data =  subset(td, is.na(td$species) == FALSE)[,c(8,2,9,10,15,16,17,18)],
            aes(colour = canopy, alpha = 0.5)) # colour them according to canopy type 
    
    # Lets inspect two variable pairs more in detail:
    ggplot(data = td, aes(x = tree.height.m, y = tree.dbh.cm,
                           col = species, shape = species)) + # obs.id is somehow inclduded in the ggplot, although it makes no sense to color it
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Tree height [m]") + ylab("Tree diameter at breast height [cm]") +
      scale_color_manual(values = canopy.colors[1:2]) # color set for canopy types as defined in line 113
    ggsave("height_dbh.pdf", height = 3.5, width = 4.5)
    
    # caluclating correlation coefficients:
    td.beech <- subset(td, td$species == "beech")
    cor(td.beech$tree.height.m, td.beech$tree.dbh.cm)
    td.spruce <- subset(td, td$species == "spruce")
    cor(td.spruce$tree.height.m, td.spruce$tree.dbh.cm)
    
    ggplot(data = td, aes(x = crown.radius.m, y = crown.beginning.m,
                           col = species, shape = species)) + # obs.id is somehow inclduded in the ggplot, although it makes no sense to color it
      geom_point() +
      geom_smooth(method = "lm") +
      xlab("Crown radius [m]") + ylab("Crown beginning [m]") +
      scale_color_manual(values = canopy.colors[1:2]) # color set for canopy types as defined in line 113
    ggsave("radius_beginning.pdf", height = 3.5, width = 4.5)
    
    # Applying a PCA and visualising colinearity in a biplot:
    names(td) # to get column numbers for selection of continuous explanatory variables
    td.sub <- subset(td, is.na(td$species) == FALSE)[,c(2,9,10,15,16,17,18)] # use only observations related to trees
    PCAres <- prcomp(td.sub, scale. = TRUE) # variables should be scaled, so set 'scale. = TRUE' ...
    # ... this computes the PCA on the correlation matrix; Without scaling variables with high values likely dominate the principal components
    PCAres$scale # scalings applied to each variable
    PCAres$center # means that were subtracted
    summary(PCAres) # get PCA summary statistics (proportional variance of each Comp)
    plot(PCAres) # shows a screeplot
    biplot(PCAres, # shows a biplot
           choices = 1:2, # select components of interest (first weight more)
           scale = 0,
           cex = c(0.5, 1.25),
           col = c("grey","black"),
           arrow.len = 0,
           xlim = c(-4,3.5)) # to see variable names in full
    # -> many variables are located in the same direction.
    #    This indicates collinearity! Applying, for example, a linear model,
    #    not all of them can be included. Use only one variable of those 
    #    that are clustered together in the biplot.
    
    # Lets look consider Variance Inflation Factors
    # Start with the full model:
    lm1 <- lm(throughfall.mm ~ gross.prec.mm + abs.distance + rel.distance + crown.beginning.m + crown.radius.m + tree.height.m + tree.dbh.cm,
              data = subset(td, is.na(td$species) == FALSE))
    vif(lm1) # Calculating VIF's (from car package)
    # ok, now lets stepwise remove the highest VIF variables
    # and lets use an a priori cut-off level of 3:
    # removing abs.distance:
    lm2 <- lm(throughfall.mm ~ gross.prec.mm + rel.distance + crown.beginning.m + crown.radius.m + tree.height.m + tree.dbh.cm,
              data = subset(td, is.na(td$species) == FALSE))
    vif(lm2)
    # -> the model is fine yet considering a cut-off of 3, although
    #    both dbh and height are still included!
    #    It is reasonable to go on and remove height or dbh:
    # removing tree.dbh.cm:
    lm3 <- lm(throughfall.mm ~ gross.prec.mm + rel.distance + crown.beginning.m + crown.radius.m + tree.height.m,
              data = subset(td, is.na(td$species) == FALSE))
    vif(lm3)
    # now all VIF's < 2
    
    # Are VIF's calculated for interactions terms?
    lm4 <- lm(throughfall.mm ~ gross.prec.mm + canopy * period, #another model with interaction
              data = subset(td, is.na(td$species) == FALSE))
    vif(lm4) # -> shows the VIF for canopy:period
    
    # Boxplots can be used to visualize interactions between continuous and
    # categorical explanatory variables
    ggplot(data = td, aes(y = crown.beginning.m)) + 
      geom_boxplot(
        aes(fill = period), # to differentiate boxplots of other period by color
        outlier.alpha = 1/3) + # (transparency for outlier points)
      scale_fill_brewer(type = "qual") +
      facet_grid(~ species + period) +
      ylab("Crown beginning [m]") +
      theme_minimal()
    ggsave("boxplot_interaction.pdf", height = 3.5, width = 5)
    
# TRANSFORMATIONS ----
    
  # * Routines for potential transformations and back transformation ----
    
    x <- c(1,2,3,4,6,9,13) # an exemplary variable
    plot(density(x)) # right skewed distribution
    
    # Logarithm
    x.log <- log(x) # calculates the natural logarithm of 'x', stored as 'x.log'
    plot(density(x.log))
    exp(x.log) # back transformation
    
    # Square root
    x.sr <- x^(1/2) # calculates the square root (the same as 'sqrt(x)')
    plot(density(x.sr))
    x.sr^2 # back transformation
    
    # Reflection
    y <- c(0, 4, 7, 9, 10, 11, 12) # another exemplary variable
    plot(density(y))
    
    log(y) # -> not possible!
    y^(1/2) # -> not meaningful!
    y.r <- (max(y) - y)^(1/2) # adding a constant of 1
    plot(density(y.r))
    y.r^(2) - max(y) # backtransformation
    # -> back-transformation is indeed right (e^-15 approx. 0), but this illustrates
    # that there can be numerical issues with any transformation!

  # * Regression example: gross precipiation vs. throughfall ----

    # the original data is skewed and high values likely dominate the regression
    # analysis. Furthermore, the variance increases with increasing mean precipitation
    # A common logarithmic transformation cannot be applied, among other for a simple
    # practical reason: log(0) becomes -Inf ! -> events without throughfall would be ignored
    # (could be solved adding 1 to the data prior to transformation)
    # A square root transformation can stabilze variances with increasing mean
    # and is able to reduce the skewness of the distribution
    # (a cube root transformation is even more strong than square root)
    
    # without transformation:
    ggplot(data = td,
           aes(x = gross.prec.mm, y = throughfall.mm)) +
      geom_point(shape = 16, size = 1/2) +
      geom_smooth(method = lm, se = FALSE, col = "black", lwd = 1/3) +
      facet_grid(canopy ~ period) +
      xlab("Gross precipiation [mm]") +
      ylab("Throughfall [mm]")
    ggsave("reg_without_transformation.pdf", width = 3.5, height = 7)
    # -> see that variance increases with gross precipitation! (this problem
    #    might be solved when more explanatory variables are considered, but
    #    considering only this factors, a transformation might solve it ...)
    # applying a square root transformation:
    ggplot(data = td,
           aes(x = gross.prec.mm^(1/2), y = throughfall.mm^(1/2))) + # square root transformation of response and explanatory variable
      geom_point(shape = 16, size = 1/2) +
      geom_smooth(method = lm, se = FALSE, col = "black", lwd = 1/3) +
      facet_grid(canopy ~ period) +
      xlab(expression(sqrt("Gross precipiation [mm]"))) +
      ylab(expression(sqrt("Throughfall [mm]")))
    ggsave("reg_sqrt_transformation.pdf", width = 3.5, height = 7)
    # -> variance at each level of gross.precipiation is similar now! Square root
    #    transformation can be applied in order to hold this important assumption.
    # applying a cubic root transformation:
    ggplot(data = td,
           aes(x = gross.prec.mm^(1/3), y = throughfall.mm^(1/3))) + # square root transformation of response and explanatory variable
      geom_point(shape = 16, size = 1/2) +
      geom_smooth(method = lm, se = FALSE, col = "black", lwd = 1/3) +
      facet_grid(canopy ~ period)
    ggsave("reg_crt_transformation.pdf", width = 3.5, height = 7)
    # -> to intense, since zero throughfall values appear as outliers
    # applying a cubic root transformation after adding 1:
    ggplot(data = td,
           aes(x = (1+gross.prec.mm)^(1/3), y = (1+throughfall.mm)^(1/3))) + # square root transformation of response and explanatory variable
      geom_point(shape = 16, size = 1/2) +
      geom_smooth(method = lm, se = FALSE, col = "black", lwd = 1/3) +
      facet_grid(canopy ~ period)
    ggsave("reg_crtplus1_transformation.pdf", width = 3.5, height = 7)
    # -> solves the zero-problem, but becomes increasingly complicated to interpret
    
    # It is also interesting to investigate the data with respect to another part
    # of the structure, namely the measurement place in form of the funnel identity.
    # Regression considering funnel.id (repeated measurements at the same place):
    ggplot(data = subset(td, td$period == "leafed"), #(sub)set of data to work with
           aes(x = gross.prec.mm^(1/2), y = throughfall.mm^(1/2), col = canopy)) + # using square root transformed data
      geom_point(stroke = 0, alpha = 0.5) + # transperant points and without border
      geom_smooth(method = lm, se = FALSE) + # applying a linear regression for every panel
      geom_abline(slope = 1, col = grey(0.5, 0.5)) + # maximum throughfall line ... points above refer to lateral inflow
      facet_wrap(funnel.id ~ .) + # creates single plots for each funnel
      scale_color_manual(values = canopy.colors) # our desired color scheme
    
    # In conclusion, it is best to use a square or cube root transformation of both variables (gross precipiation and
    # throughfall), since this overcomes problems of non-homogeneous variances and the right-tailed
    # distribution, finally enabling to investigate their relationship by linear regression.