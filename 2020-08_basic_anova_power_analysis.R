# Power analysis taken from https://stats.idre.ucla.edu/r/dae/one-way-anova-power-analysis/
groupmeans = c(550, 598, 598, 646)
power.anova.test(groups = length(groupmeans), 
                 between.var = var(groupmeans), 
                 within.var = 6400, 
                 power=0.8, sig.level=0.05,n=NULL) 

# Calculate power through simulation as proof of concept
anova_testfun = function(ngroups, n) {
     groups = rep(letters[1:ngroups], times = n)
     y = rnorm(ngroups*n, mean = groupmeans, sd = sqrt(6400) )
     anova(lm(y ~ groups))$`Pr(>F)`[1]
}
anova_testfun(ngroups = 4, n = 17)

allp = replicate(n = 1000, 
                 expr = anova_testfun(ngroups = 4, n = 17),
                 simplify = TRUE)
mean(allp < .05)

# Now make this more specific to current Elliott design

# Vector of names of treatments
trtnames = c("Intensive", "Triad E", "Triad I", "Extensive")
trtn = c(9, 11, 10, 10) # Number per group (same order as names)
groupmeans = c(550, 598, 598, 646) # These means can change
names(groupmeans) = trtnames # Add names to extract largest difference
groupvar = c(6000, 6400, 6400, 6800) # Allow variances to differ per group

# Calculate groups that make up largest and smallest means
# (so biggest difference)
# If have ties, take the first name alphabetically
minmean = names(groupmeans[groupmeans == min(groupmeans)])[1]
maxmean = names(groupmeans[groupmeans == max(groupmeans)])[1]

# Make data
trt = rep(trtnames, times = trtn) # Repeat treatments
# For y to match trt order need to loop through 
# trtn, groupmeans, and groupvar
library(purrr)
# Using unlist() t get in vector
set.seed(16)
y = pmap(list(trtn, groupmeans, groupvar),
         function(n, mean, var) {
              rnorm(n = n, mean = mean, sd = sqrt(var) )
         } ) %>%
     unlist()

# I decided to put these in a data.frame
# to set factor level order
dat = data.frame(trt = forcats::fct_inorder(trt), y)

# Fit model
model = lm(y ~ trt, data = dat)

# Extract overall F test p-value
anova(model)$`Pr(>F)`[1]


# Extract estimated difference for 
     # the two groups that were defined to have largest diff
library(emmeans)
res = as.data.frame( emmeans(model, pairwise ~ trt)$contrasts )
# Get only the row for minmean vs maxmean
res = res[grepl(paste0(minmean, " - ", maxmean), res$contrast) |
               grepl(paste0(maxmean, " - ", minmean), res$contrast),]
res$estimate # I just want the estimate for now

# The emmeans code turned out to be very slow
# Switch to manually pulling out coefficients since
     # I'm only interested in the estimated difference
     # between the min and max defined means right now
coefs = coef(model)
coefs[2:4] = coefs[2:4] + coefs[1]
names(coefs) = groups
est = coefs[maxmean] - coefs[minmean]

# Make a function to return overall p-value and
     # estimate for largest diff
library(purrr)
# I skipped using emmeans because it added
     # a lot of time
anova_fun = function(groups, n, means, vars) {
     trt = rep(groups, times = n)
     y = unlist( pmap(list(n, means, vars),
              function(n, mean, var) {
                   rnorm(n = n, mean = mean, sd = sqrt(var) )
              } ) )
     
     dat = data.frame(trt = forcats::fct_inorder(trt), y)
     model = lm(y ~ trt, data = dat)
     
     coefs = coef(model)
     coefs[2:4] = coefs[2:4] + coefs[1]
     names(coefs) = groups
     est = coefs[maxmean] - coefs[minmean]
     
     results = data.frame(p = anova(model)$`Pr(>F)`[1],
                          est)
     names(results) = c("p", paste(maxmean, "minus", minmean) )
     results
}
anova_fun(groups = trtnames,
          n = trtn,
          means = groupmeans,
          vars = groupvar)

allres = replicate(n = 1000, 
                   expr = anova_fun(groups = trtnames,
                                    n = trtn,
                                    means = groupmeans,
                                    vars = groupvar),
                   simplify = FALSE)

allres = do.call("rbind", allres)

# Power
mean(allres$p < .05)

# Distribution of effects

# Calculate true difference based on given means
# I did max minus min throughout
truediff = groupmeans[maxmean] - groupmeans[minmean]

library(ggplot2)
library(ggtext)
ggplot(data = allres, aes(x = .data[[names(allres)[2]]]) ) +
     geom_density(fill = "blue") +
     geom_vline(xintercept = truediff) +
     annotate("label", label = paste("True difference: ", truediff),
              x = truediff, y = 0, angle = 90) +
     labs(title = "Distribution of estimated difference in means",
          subtitle = paste0("*", names(allres)[2], "*"),
          caption = "*Results from 1000 simulations*",
          y = "Density",
          x = NULL) +
     theme_bw(base_size = 18) +
     theme(axis.text.y = element_blank(),
           axis.ticks.y = element_blank(),
           plot.caption = element_markdown(),
           plot.subtitle = element_markdown() )


# apply() vs pmap(); pmap() seems better
groupdat = data.frame(trtn, groupmeans, groupvar)
set.seed(16)
microbenchmark::microbenchmark( unlist( apply(data.frame(trtn, groupmeans, groupvar), MARGIN = 1, FUN = function(x) {
     rnorm(n = x[1], mean = x[2], sd = sqrt(x[3]) )
} ) ) )

microbenchmark::microbenchmark(unlist(pmap(groupdat,
     function(trtn, groupmeans, groupvar) {
          rnorm(n = trtn, mean = groupmeans, sd = sqrt(groupvar) )
     } ) ) )

microbenchmark::microbenchmark(unlist(pmap(list(trtn, groupmeans, groupvar),
                                           function(trtn, groupmeans, groupvar) {
                                                rnorm(n = trtn, mean = groupmeans, sd = sqrt(groupvar) )
                                           } ) ) )
