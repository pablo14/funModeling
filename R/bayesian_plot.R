#' @title Cross-plotting input variable vs. target variable
#' @description The bayesian_plot shows how the input variable is correlated with the target variable, comparing with a bayesian approach the posterior conversion rate to the target variable. It's useful to compare categorical values, which have no intrinsic ordering. If they have instrinsic ordering, cross_plot can be used instead.
#' @param data data frame source
#' @param input string input variable (if empty, it runs for all variables), it can take a single character value or a character vector.
#' @param target string of the variable to predict
#' @param title Plot title
#' @param plot_all By default, if there are more than 10 unique values, the plot will only show the top and bottom 5 values, and conglomerate the rest in "Other categories".
#' @param extra_above How many of the most likely occurrences to plot above
#' @param extra_under How many of the least likely occurrences to plot above
#' @param path_out path directory, if it has a value the plot is saved
#' @examples
#' \dontrun{
#' ## Example 1:
#' bayesian_plot(data=heart_disease, input="chest_pain", target="has_heart_disease")
#'
#' ## Example 2: Saving the plot into a folder:
#' bayesian_plot(data=heart_disease, input="oldpeak",
#' 		target="has_heart_disease", path_out = "my_folder")
#'
#' ## Example 3: Running with multiple input variables at the same time:
#' bayesian_plot(data=heart_disease, input=c("gender", "age"),
#' 		target="has_heart_disease", path_out = ".")
#'}
#' @return bayesian plot
#' @export
bayesian_plot <- function(data, input, target, title="Bayesian comparison", plot_all=F, extra_above=5, extra_under=5, path_out){

	## Handling missing parameters
	if(missing(path_out)) path_out=NA

	## If str_input then runs for all variables
	if(missing(input))
	{
		## Excluding target variable
		input=colnames(data)
		input=input[input!=target]
	}

	## Iterator
	for(i in 1:length(input))
	{
		b = bayesian_plot_logic(data, input[i], target, title, plot_all, extra_above, extra_under, path_out)
	}
	b
}


bernoulli_posteriors = function(alpha, beta, data, categ) {
	#Draw posteriors out of a beta distribution + the observed data
	set.seed(4354343)
	probs <- rbeta(1e5, sum(data) + alpha, length(data) - sum(data) + beta)
	return(data.frame(value=probs, categ=rep(ifelse(categ=='',"Unknown",categ), length(probs))))
}

beta_ab_from_ms = function( mean , sd ) {
	#From a mean + SD, get the alpha and beta of the beta distribution
	if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
	if ( sd <= 0 ) stop("sd must be > 0")
	kappa = mean*(1-mean)/sd^2 - 1
	if ( kappa <= 0 ) stop("invalid combination of mean and sd")
	a = mean * kappa
	b = ( 1.0 - mean ) * kappa
	return( list( a=a , b=b ) )
}

bayesian_plot_logic = function(df, str_input, str_target, title, plot_all, extra_above, extra_under, path_out){
	if(is.factor(df[[str_input]]) | is.numeric(df[[str_input]])){
		df[[str_input]] = as.character(df[[str_input]])
	}
	input = df[, as.character(str_input)]

	if(is.character(df[[str_target]]) | is.factor(df[[str_target]])){
		df[[str_target]] = as.numeric(as.factor(df[[str_target]]))-1
	}
	target = df[, as.character(str_target)]

	prior = round(length(which(target == 1)) / length(which(!is.na(target))), 6)
	betaDist = beta_ab_from_ms(prior, prior*0.15)
	alpha = as.numeric(betaDist$a)
	beta = as.numeric(betaDist$b)


	if(sum(is.na(df[str_input])) > 0){
		warning("NAs found and coerced to <NA>")
		df[str_input] = ifelse(is.na(input), '<NA>',input)
	}

	values = unique(df[str_input])
	values <- values[!is.na(values)]

	if(length(values) < 10 | plot_all==T){
		#Simple
		posteriors = data.frame(value=numeric(), categ=character())
		for(i in 1:length(values)){
			onlyThisValue = df[df[str_input] == values[i] & !is.na(df[str_input]), ][[str_target]]
			onlyThisValue = onlyThisValue[!is.na(onlyThisValue)]
			posteriors = rbind(posteriors, bernoulli_posteriors(alpha, beta, onlyThisValue, values[i]))
		}
	}else{
		nonrep = dplyr::group_by_(df, .dots=as.symbol(str_input)) %>%
			dplyr::count() %>%
			filter(n < 5) # 5 is a thumb rule

		noconv = dplyr::group_by_(df, .dots=as.symbol(str_input)) %>%
			dplyr::summarize_(sum_pos=lazyeval::interp(~sum(var), var=as.name(str_target))) %>%
			filter(sum_pos==0)

		others=  unique(data.frame(rbind(nonrep[,1], noconv[,1])))
		likelihoods = dplyr::anti_join(df, others, by=str_input) %>%
			dplyr::group_by_(.dots=as.symbol(str_input)) %>%
			dplyr::summarize_(likelih=interp(~sum(var) /sum(var == 0), var=as.name(str_target))) %>%
			dplyr::arrange(desc(likelih))

		top = data.frame(head(likelihoods, extra_above))[,1]
		bottom = data.frame(tail(likelihoods, extra_under))[,1]
		valuesInteresting = c(top, bottom)

		posteriors = data.frame(value=numeric(), categ=character())
		for(i in 1:length(valuesInteresting)){
			onlyThisValue = df[df[str_input] == valuesInteresting[i] & !is.na(df[str_input]), ][[str_target]]
			posteriors = rbind(posteriors, bernoulli_posteriors(alpha, beta, onlyThisValue, values[i]))
		}

		onlyThisValue = df[df[str_input] %in% others & !is.na(df[str_input]), ][[str_target]]
		posteriors = rbind(posteriors, bernoulli_posteriors(alpha, beta, onlyThisValue, 'Other categories'))


	}


	p <- ggplot2::ggplot(posteriors, aes_string(x = "value", group="categ", colour="categ", fill="categ")) +
		ggplot2::geom_density(alpha = 0.05) +
		ggplot2::labs(x=paste0("Probability of ",str_target), y="Density") +
		ggplot2::ggtitle(title) +
		theme_bw()

	if(!is.na(path_out))
	{

		dir.create(path_out, showWarnings = F)

		if(dir.exists(path_out))
		{
			jpeg(sprintf("%s/%s.jpeg", path_out, str_input), width= 12.25, height= 6.25, units="in",res=200, quality = 90)

			plot(p)
			dev.off()
		} else {
			warning(sprintf("The directory '%s' doesn't exist.", path_out))
		}
	}
	p
}
