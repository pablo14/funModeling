utils::globalVariables(names=c("fum","element_blank","value","ratio","aes","variable","geom_bar","geom_text","position","guides","labs","theme","element_text","scale_y_continuous","position_dodge","ylim","guide_legend","scale_fill_discrete"),package = "funModeling", add = F)


#' @importFrom grDevices dev.off jpeg rainbow
#' @importFrom graphics abline grid
#' @importFrom stats predict
#' @importFrom pander pandoc.table
#' @importFrom  Hmisc cut2
#' @importFrom  ggplot2 ggplot
#' @import  plyr
#' @importFrom reshape2 dcast melt
#' @importFrom scales percent
#' @importFrom gridExtra grid.arrange
#' @importFrom ROCR prediction performance plot
#'
#' @title Cross-plotting input variable vs. target variable
#' @description The cross_plot shows how the input variable is correlated with the target variable, getting the likelihood rates for each input's bin/bucket .
#' @param data data frame source
#' @param str_input string input variable
#' @param str_target string of the variable to predict
#' @param path_out path directory, if it has a value the plot is saved
#' @param auto_binning indicates the automatic binning of str_input variable based on equal frequency (function 'equal_freq'), default value=TRUE
#' @examples
#' ## Example 1:
#' cross_plot(data=heart_disease, str_input="chest_pain", str_target="has_heart_disease")
#'
#' ## Example 2:
#' cross_plot(data=heart_disease, str_input="age", str_target="has_heart_disease")
#'
#' @return cross plot
#' @export
cross_plot <- function(data, str_input, str_target, path_out, auto_binning)
{
	## Declaring variables/function as global for CRAN check
	#globalVariables(names=c("fum","element_blank","value","ratio","aes","variable","geom_bar","geom_text","position","guides","labs","theme","element_text","scale_y_continuous","position_dodge","ylim","guide_legend","scale_fill_discrete"))



	## Handling missing parameters
  if(missing(auto_binning)) auto_binning=T
  if(missing(path_out)) path_out=NA

  ## Checking for variable existance.
  if(!(str_target %in% colnames(data))) stop(sprintf("Target variable '%s' does not exists", str_target))
  if(!(str_input %in% colnames(data))) stop(sprintf("Input variable '%s' does not exists", str_input))

  ## Removing NA from target variable #########
  data_tmp=subset(data, !is.na(data[, str_target]))
  if(nrow(data) > nrow(data_tmp))
  {
    warning(sprintf("There were removed %d rows with NA values in target variable '%s'.", nrow(data)-nrow(data_tmp), str_target))

    ## Keeping with cleaned data
    data=data_tmp
  }
  #############################################

  ## Initial assignments
  target=data[, as.character(str_target)]
  varInput=data[, as.character(str_input)]

  ## Stop if target is not binary #############
  if(length(unique(target))>2)
  {
    stop(sprintf("Target variable '%s' does not have 2 unique values.", str_target))
  }
  #############################################

  ## Auto binning #############################
  if(auto_binning & is.numeric(varInput) & length(unique(varInput))>20)
  {
    print(sprintf("Plotting transformed variable '%s' with 'equal_freq', (too many values). Disable with 'auto_binning=FALSE'", str_input))
    varInput=suppressWarnings(equal_freq(varInput, 10))
  }
  #############################################


  ## Infer the less representative class (commonly the one to predict)
  dcount=data.frame(count(target))
  posClass=as.character(dcount[order(dcount$freq),][1,1])
  negClass=as.character(dcount[order(dcount$freq),][2,1])

  dataCast = dcast(data,varInput~target,fun.aggregate=length, value.var = str_target)

  ## Melt data for ggplot
  dataMelt=melt(dataCast, measure.vars = c(posClass,negClass))

  ## Converting target into factor
  dataMelt$variable=factor(dataMelt$variable, c(posClass,negClass))

  ## Getting percentage numbers
  m1 = ddply(dataMelt, "varInput", plyr::summarize, ratio=value/sum(value))

  ## Order by var input
  m2 = dataMelt[order(dataMelt$varInput ),]
  dataGrafPrep=data.frame(m2,ratio=m1$ratio)

  ## Generating Id indicator for odd/even calculus
  rownames(dataGrafPrep) = 1:nrow(dataGrafPrep)

  ## Computing if the row is odd/even
  dataGrafPrep$fum=as.numeric(as.numeric(rownames(dataGrafPrep)) %% 2 == 0)

  ## Computing middle position in each sub bar
  dataGrafPrep=ddply(dataGrafPrep, .(varInput), transform, position = 0.5*ratio+fum*(sum(value)-value)/sum(value))

  lGraf=list()

  ## Percentual
  lGraf$percentual =  ggplot(dataGrafPrep, aes(x=factor(varInput), y=value, fill=variable))+geom_bar(position="fill",stat="identity") +
    geom_text(aes(label = sprintf("%0.1f", 100*ratio), y = position)) +
    guides(fill=FALSE) + labs(x = str_input, y = paste(str_target, " (%)", sep=" ")) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    scale_y_continuous(labels=percent)


    ## Quantity plot
  lGraf$quantity =  ggplot(dataGrafPrep, aes(x=factor(varInput), y=value, ymax=max(value)*1.05, fill=variable)) + geom_bar(position=position_dodge(),stat="identity") +
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25, size=4) +
    labs(x = str_input, y = paste(str_target, " (count)", sep=" ")) +
    ylim(0, max(dataGrafPrep$value)+max(dataGrafPrep$value)*0.05) +
    theme(axis.text.x=element_text(angle = 45, hjust = 1),legend.title=element_blank()) +
    guides(col = guide_legend(ncol = 1, byrow = TRUE)) +
    scale_fill_discrete(name=str_target)


  ## Printing both plots
  final_plot=grid.arrange(lGraf$percentual, lGraf$quantity, ncol=2)

  ## Save plot
  if(!missing(path_out) & !is.na(path_out))
  {
    if(dir.exists(path_out))
    {
      jpeg(sprintf("%s/%s.jpeg", path_out, str_input), width= 12.25, height= 6.25, units="in",res=200, quality = 90)

      plot(final_plot)
      dev.off()
    } else {
      warning(sprintf("The directory '%s' doesn't exists.", path_out))
    }
  }
}

#' Equal frequency binning
#' @description Equal frequency tries to put the same quantity of cases per bin when possible. It's a wrapper of function cut2 from Hmisc package.
#' @param var input variable
#' @param n_bins number of bins to split 'var' by equal frequency, if it not possible to calculate for the desired bins, it returns the closest number
#' @examples
#' ## Example 1
#' summary(heart_disease$age)
#' age_2=equal_freq(var=heart_disease$age, n_bins = 10)
#' summary(age_2)
#'
#' ## Example 2
#' age_3=equal_freq(var=heart_disease$age, n_bins = 5)
#' summary(age_3)
#' @return The binned variable.
#' @export
equal_freq <- function(var, n_bins)
{
  n_bins_orig=n_bins

  ## Call the cut2 from Hmisc
  res=cut2(var, g = n_bins)

  n_bins_final=length(unique(res))

  if(n_bins_final != n_bins)
    warning(sprintf("It's not possible to calculate with n_bins=%s, setting n_bins in: %s.", n_bins, n_bins_final))

  return(res)
}


#' @title Generate cross_plot for all variables
#' @description Internal function used in massive_cross_plot to generate as many cross_plot as elements are in str_vars.
#' @param str_vars string vector containing variable names of 'data' param.
#' @param data data frame source
#' @param str_target string of the variable to predict
#' @param path_out path directory, if it has a value the plot is saved
#' @param auto_binning indicates the automatic binning of str_input variable based on equal frequency (function 'equal_freq'), default value=TRUE
cross_iterator <- function (str_vars, data, str_target, path_out, auto_binning) {
  for(i in 1:length(str_vars))
  {
    cross_plot(data = data, str_input=str_vars[i], str_target=str_target, path_out = path_out, auto_binning)
  }
}

#' @title Generate several cross_plot at the same time.
#' @description It creates as many cross_plots as variable names are present in 'str_vars'. Additionally, they can be saved as jpeg in a folder (setting 'path_out' parameter).
#' @param data data frame source
#' @param str_target string of the variable to predict
#' @param str_vars vector of strings containing the variables names to be used as input in each cross_plot.
#' @param path_out path directory, if it has a value the plot is saved in 'path_out'
#' @examples
#' ## Example 1
#' massive_cross_plot(data=heart_disease,
#' str_target="has_heart_disease",
#' str_vars=c("age", "oldpeak", "max_heart_rate"))
#'
#' ## Example 2: Save all the plots to a new folder "plots_example" inside the working directory.
#' massive_cross_plot(data=heart_disease, str_target="has_heart_disease",
#' str_vars=c("age", "oldpeak", "max_heart_rate"),
#' path_out="plots_example")
#'
#' @param auto_binning indicates the automatic binning of str_input variable based on equal frequency (function 'equal_freq'), default value=TRUE
#' @export
massive_cross_plot <- function (data, str_target, str_vars, path_out, auto_binning)
{

  if(missing(path_out)) {
     path_out=NA
     cross_iterator(str_vars=str_vars, data=data, str_target=str_target, path_out=path_out, auto_binning=auto_binning)
  } else {

    dir.create(path_out, showWarnings = F)

    if(dir.exists(path_out)) {
     cross_iterator(str_vars=str_vars, data=data, str_target=str_target, path_out=path_out, auto_binning=auto_binning)

    } else {
      warning(sprintf("The directory '%s' cannot be created", path_out))
    }
  }
}

