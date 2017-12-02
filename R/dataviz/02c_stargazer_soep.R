# Stargazer Introduction: 

#install.packages("stargazer")
library(stargazer)


# Models SOEP

mod1 <- lm(netinc ~ alter ,data = SOEP)
summary(mod1)

mod2 <- lm(netinc ~ alter + sex,data = SOEP)
summary(mod2)


# Nice Apa-Ready Output: 

stargazer(mod1,mod2,  #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Hier steht die Überschrift",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "table1_SOEP.html", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("basic model","2 variable model"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Alter",  # Covariate Labels
                               "Geschlecht",
                               "Intercept"),
          dep.var.caption = "Abh. Var", # Caption (Top) of dependent variable
          star.cutoffs = c(0.05,0.01,0.001),
          dep.var.labels = c("Netincome"))



# Voller Optionenkanon:

stargazer(mod1,mod2,  #regression models 
          type = "html", # character vector (eg. "text" / "html" / "latex")
          title = "Hier steht die Überschrift",  # header
          style = "default",  # style (choice see below)
          summary = NULL,  # logical vector: output summary statistics when given data.frame
          out = "table2_SOEP.html", # path and output of file
          out.header = FALSE, # logical vector: should output file contain code-header?
          column.labels = c("basic model","2 variable model"), # column labels for mod1/mod2
          column.separate = c(1,1),  # how column labels should be assigned (label over sev. columns possible)
          covariate.labels = c("Alter",  # Covariate Labels
                               "Geschlecht",
                               "Intercept"),
          dep.var.caption = "Abh. Var", # Caption (Top) of dependent variable
          dep.var.labels = c("Nettoeinkommen"), # dependent variable label
          dep.var.labels.include = TRUE, # include them?
          ci = FALSE, # return confidence intervals?
          ci.level = 0.95, # confidence interval level
          ci.separator = "|", # character separator
          column.sep.width = "5pt", # in latex: width between columns
          decimal.mark = ",", # decimal mark character string (z.b. comma)
          df = TRUE, # degrees of freedom reported?
          digit.separate = NULL, # where to seperate digits (vector with first element number of digits left of comma that will be seperated, second # of d. sep. from that first sep.)
          digit.separator = NULL, # character string
          digits = 3, # decimals before rounding
          digits.extra = 4, # maximum extra if rounded number is 0
          flip = FALSE, # flip axes when printing summary statistics
          float = TRUE,  # resulting table is floating table?
          float.env="table", # only latex-relevant
          font.size = NULL, # it's huuuuuge: "tiny", "scriptsize", "footnotesize", "small", "normalsize", "large", "Large", "LARGE", "huge", "Huge"
          header = TRUE, # latex
          initial.zero = TRUE, # initial zero before decimal mark 
          intercept.bottom = TRUE, # intercept at the bottom
          intercept.top = FALSE, 
          keep = NULL, # which explanatory vars. to keep
          keep.stat = c("n","rsq","f"), #which model statistics to keep (LIST SEE BELOW)
          label = "", # latex
          model.names = TRUE, # include model name? (OLS/Probit etc)
          model.numbers = TRUE, # number models?
          multicolumn = TRUE, # report over sev. columns if same
          no.space = NULL, # remove empty lines?
          notes = "hello this is a message from the people from friedrichshafen", # notes to be included below table
          notes.align = "l", # l = left | r = right | c = center
          notes.append = TRUE, 
          notes.label = "PS",  # Label of Notes section
          object.names = FALSE, # indicate objects in table?
          omit = NULL, # omit explanatory (see. keep)
          omit.labels = NULL, 
          omit.stat = NULL, # see keep
          omit.summary.stat = NULL,
          omit.table.layout = NULL,
          omit.yes.no = c("Yes", "No"), 
          order = NULL, # Order of variables in output (watch out for labelling)
          ord.intercepts = FALSE, # for ordered probit/logit
          perl = FALSE, # use perle-compatible expressions?
          report = NULL, # character string containing only elements of "v", "c", "s","t", "p", "*" that determines whether, and in which order, variable names ("v"), coefficients ("c"), standard errors/confidence intervals ("s"), test statistics ("t") and p-values ("p") should be reported in regression tables.
          rownames = NULL, # for df,c,matrices
          rq.se = "nid", # method for standard error calculation: "iid", "nid", "ker" and "boot".
          selection.equation = FALSE, # selection or outcome equation for heckit models
          single.row = FALSE, # regression and s.e. in same row?
          star.char = "*", # character string to be used as star
          star.cutoffs = c(0.05,0.01,0.001), # vector containing star cutoff levels
          suppress.errors = FALSE, # supress error messages`?`
          table.layout = NULL, # table layout characters see below
          table.placement = "!htbp", # latex
          zero.component = FALSE, # only zeroinfl/hurdle estimation models
          summary.logical = TRUE, # logical variables in summary statistics?
          summary.stat = NULL, # character vector of summary statistics (see codes below)
          nobs = TRUE, # show n in each column?
          mean.sd = TRUE, # show mean and sd?
          min.max = TRUE, # show min/max?
          median = FALSE, # show median?
          iqr = FALSE ) # quantiles




# List of supported styles: 

# "all"	 publish every statistic available, incl. t-statistics and p-values
# "all2"	 same as "all", but omitting t-statistics and p-values
# "default"	 default: publish regression coefficients with standard errors, and the most commonly reported statistics
# "commadefault"	 like "default", but uses a decimal comma and a single space to separate thousands
# "aer"	 American Economic Review
# "ajps"	 American Journal of Political Science
# "ajs"	 American Journal of Sociology
# "asq"	 Administrative Science Quarterly
# "asr"	 American Sociological Review
# "apsr"	 American Political Science Review
# "demography"	 Demography
# "io"	 International Organization
# "jpam"	 Journal of Policy Analysis and Management
# "qje"	 Quarterly Journal of Economics
# 


# for statistic codes to print, see: 
# "all"	 all statistics
# "adj.rsq"	 adjusted R-squared
# "aic"	 Akaike Information Criterion
# "bic"	 Bayesian Information Criterion
# "chi2"	 chi-squared
# "f"	 F statistic
# "ll"	 log-likelihood
# "logrank"	 score (logrank) test
# "lr"	 likelihood ratio (LR) test
# "max.rsq"	 maximum R-squared
# "n"	 number of observations
# "null.dev"	 null deviance
# "Mills"	 Inverse Mills Ratio
# "res.dev"	 residual deviance
# "rho"	 rho
# "rsq"	 R-squared
# "scale"	 scale
# "theta"	 theta
# "ser"	 standard error of the regression (i.e., residual standard error)
# "sigma2"	 sigma squared
# "ubre"	 Un-Biased Risk Estimator
# "wald"	 Wald test

# Table layout characters: 

# "-"	 single horizontal line
# "="	 double horizontal line
# "-!"	 mandatory single horizontal line
# "=!"	 mandatory double horizontal line
# "l"	 dependent variable caption
# "d"	 dependent variable labels
# "m"	 model label
# "c"	 column labels
# "#"	 model numbers
# "b"	 object names
# "t"	 coefficient table
# "o"	 omitted coefficient indicators
# "a"	 additional lines
# "n"	 notes
# "s"	 model statistics


# Summary statistics codes

# "max"	 maximum
# "mean"	 mean
# "median"	 median
# "min"	 minimum
# "n"	 number of observations
# "p25"	 25th percentile
# "p75"	 75th percentile
# "sd"	 standard deviation