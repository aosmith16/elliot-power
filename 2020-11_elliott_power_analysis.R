# 2020-11
# Start first draft of actual power analysis for many response variables

# R packages ----
library(readxl) # Read excel data
library(purrr) # Looping
library(ggplot2) # Plotting
library(ggalt) # plotting lollipops
library(ggtext) # plotting text
library(dplyr) # Data manip (summarizing)
library(tidyr) # Reshaping

# Dataset ----
# The dataset came in 2020-11-03 from Scott Harris
    # and is stored in the data folder in the project directory

# It is not ready for the power analysis
dat = read_excel(here::here("data", "2020-11-03_ESRF_responses.xlsx"), sheet = 2)

# The response variables of interest are:
    # CARBac = carbon (live and dead trees including roots) in Mg/acre
    # BTYW10 = relative abundance of black-throated gray warbler per 10 acres
    # GCKI10 = relative abundance of golden-crowned kinglet per 10 acres
    # HEWA10 = relative abundance of hermit warbler per 10 acres
    # OCWA10 = relative abundance of orange-crowned warbler per 10 acres
    # RUHU10 = relative abundance of rufous hummingbird per 10 acres
    # WIFL10 = relative abundance of willow flycatcher per 10 acres
    # WIWA10 = relative abundance of Wilson's warbler per 10 acres
    # MAMU10 = relative abundance of marbled murrelet  per 10 acres

# Get these plus watershed, treatment, and period
resps = c("CARBac", 								
          "BTYW10", 
          "GCKI10",
          "HEWA10",
          "OCWA10",
          "RUHU10",
          "WIFL10",
          "WIWA10",
          "MAMU10")

# Keep lower case bird names as assuming
    # I'll put "relative abundance" before in plots and "per 10 acres" after
respnames = c("Carbon (Mg/acre)",
              "Black-throated gray warbler",
              "golden-crowned kinglet",
              "hermit's warbler",
              "orange-crowned warbler",
              "rufous hummingbird",
              "willow flycatcher",
              "Wilson's warbler",
              "marbled murrelet")
names(respnames) = resps

# Per Scott, don't use bird names in axis labels
    # so make a second vector of just the 4 character codes
    # for names of saved plots
respnames2 = c("Carbon", 								
          "BTYW", 
          "GCKI",
          "HEWA",
          "OCWA",
          "RUHU",
          "WIFL",
          "WIWA",
          "MAMU")
names(respnames2) = resps

dat = select(dat, wshed, treatment, period, all_of(resps) )


# Need to do some calculations
# Per Scott
# "For the birds, the comparison should be cumulative over X periods minus period 0." 
# "For example, sum periods 1 through 10 then subtract period 0, for the period 10 analysis"
    # This indicates a cumulative sum per watershed and treatment minus 2*period 0 
    # (since is included in cumsum)
# "For carbon, the comparision will be a "snapshot"." 
# "For example, period 10 carbon minus period 0 carbon, for the period 10 analysis"

respdat = dat %>% 
    group_by(wshed, treatment) %>%
    mutate(across(resps[2:9], ~cumsum(.x) - 2*.x[period == 0]),
           CARBac = CARBac - CARBac[period == 0]) %>%
    ungroup()

# Will need better names for things in plots and tables
# Fix treatment names
treat = data.frame(treatment = unique(respdat$treatment),
                   Treatment = c("Triad-I", "Triad-E", "Intensive", "Extensive") )
respdat = left_join(treat, respdat, by = "treatment") %>%
    select(-treatment)

# Periods should be replaced by years (period * 5)
# Then remove period
# Also set factor order of Treatment
respdat = mutate(respdat, 
                 Year = period*5,
                 period = NULL,
                 Treatment = factor(Treatment, levels = c("Extensive", "Triad-E",
                                                          "Triad-I", "Intensive")))

# Remove 0 year rows before saving
respdat = filter(respdat, Year != 0)

# Save this out
# write.csv(select(respdat, Treatment, Year, everything()),
#           file = here::here("data", "2020-11_ESRF_power_analysis_responses.csv"),
#           row.names = FALSE)


# Reshape data into long format
respdat_long = pivot_longer(respdat, cols = all_of(resps) )

# Change "key" names via merging, use code names not title names
dat_respnames = data.frame(name = resps,
                            response =  respnames2)

respdat_long = left_join(dat_respnames, respdat_long, by = "name") %>%
    select(-name)

# Calculate sample size, means and variance per response, treatment, and year ----
sumdat = respdat_long %>%
    group_by(response, Treatment, Year) %>%
    summarise(n = n(),
              mean = mean(value),
              variance = var(value),
              .groups = "drop")

# Save this out
# write.csv(sumdat,
#           file = here::here("data", "2020-11_ESRF_power_analysis_means_and_variances.csv"),
#           row.names = FALSE)


# Make boxplots per response variable ----
# Colors from Scott, I used rgb(#, #, #, maxColorValue = 255) to get hex
# Extensive, orange RGB 251-169-25, "#FBA919"
# Triad-E, pinkish RGB 245-191-215, "#F5BFD7"
# Triad-I, blue RGB 53-195-243, "#35C3F3"
# Intensive, green RGB 175-209-53, "#AFD135"
colors = c("Extensive" = "#FBA919",
           "Triad-E" = "#F5BFD7",
           "Triad-I" = "#35C3F3",
           "Intensive" = "#AFD135")

# Focus on periods 4, 10, 20 (which is years 20, 50, 100)
respdat_years = filter(respdat, Year %in% c(20, 50, 100) )

# Labels for facets
# No facets needed per Scott
# fac_labels <- c('20' = "Year 20", 
#                 '50' = "Year 50", 
#                 '100' = "Year 100")

# May want to set limits of boxplots all the same
# Test with OCWA
ocwalims = range(pull(respdat_years, OCWA10))


# Test boxplot for one response for one year
ggplot(data = filter(respdat_years, Year == 20), aes(x = Treatment, y = OCWA10) ) +
    geom_boxplot(aes(color = Treatment)) +
    geom_point(aes(color = Treatment), size = 2) +
    scale_color_manual(values = colors, guide = "none") +
    # scale_fill_manual(values = colors, guide = "none")  +
    scale_y_continuous(limits = ocwalims) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 3.5, color = "grey24") +
    # facet_wrap(~Year, scales = "free_y", labeller = labeller(Year = fac_labels)) +
    theme_bw(base_size = 12) +
    labs(y = "Birds per 10 acres",
         x = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(color = "black") )

# Test saving for save and general look
# ggsave("testbox1.png", path = here::here("plots"),
#        width = 4, height = 3, dpi = 300)

ggplot(data = filter(respdat_years, Year == 50), aes(x = Treatment, y = OCWA10) ) +
    geom_boxplot(aes(color = Treatment)) +
    geom_point(aes(color = Treatment), size = 2) +
    scale_color_manual(values = colors, guide = "none") +
    # scale_fill_manual(values = colors, guide = "none")  +
    scale_y_continuous(limits = ocwalims) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 3.5, color = "grey24") +
    # facet_wrap(~Year, scales = "free_y", labeller = labeller(Year = fac_labels)) +
    theme_bw(base_size = 12) +
    labs(y = "Birds per 10 acres",
         x = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(color = "black") )

# ggsave("testbox2.png", path = here::here("plots"),
#        width = 4, height = 3, dpi = 300)

ggplot(data = filter(respdat_years, Year == 100), aes(x = Treatment, y = OCWA10) ) +
    geom_boxplot(aes(color = Treatment)) +
    geom_point(aes(color = Treatment), size = 2) +
    scale_color_manual(values = colors, guide = "none") +
    # scale_fill_manual(values = colors, guide = "none")  +
    scale_y_continuous(limits = ocwalims) +
    stat_summary(fun = mean, geom = "point", shape = 18,
                 size = 3.5, color = "grey24") +
    # facet_wrap(~Year, scales = "free_y", labeller = labeller(Year = fac_labels)) +
    theme_bw(base_size = 12) +
    labs(y = "Birds per 10 acres",
         x = NULL) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text = element_text(color = "black") )

# ggsave("testbox3.png", path = here::here("plots"),
#        width = 4, height = 3, dpi = 300)

# Pad year with 0 for naming saved plots
respdat_years$Year = sprintf("%03d", respdat_years$Year)

# Add nicer names to list of names to loop through
names(resps) = respnames2

# Split dataset by year
year_split = split(respdat_years, respdat_years$Year)

# Function for boxplot
boxplot_fun = function(data, yvar) {
    
    if(yvar == "CARBac") {
        ylab = "Carbon (Mg/acre)"
    } else {
        ylab = "Birds per 10 acres"
    }
    ggplot(data = data, aes(x = Treatment, y = .data[[yvar]]) ) +
        geom_boxplot(aes(color = Treatment)) +
        geom_point(aes(color = Treatment), size = 2) +
        scale_color_manual(values = colors, guide = "none") +
        stat_summary(fun = mean, geom = "point", shape = 18,
                     size = 3.5, color = "black") +
        theme_bw(base_size = 12) +
        labs(y = ylab,
             x = NULL) +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text = element_text(color = "black") )
}

# Test function
boxplot_fun(data = year_split[[1]], yvar = resps[[1]])


all_plots = map(year_split, 
               ~map(resps, function(yvar) {
                   boxplot_fun(data = .x, yvar = yvar)
               }) )


# Make names for all the plots then flatten the list
# From https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/#saving-all-plots-separately
plotnames = imap(all_plots, ~paste0("2020-11_ESRF_boxplot_", names(.x), "_", "year", .y, ".png")) %>%
    flatten()
plotnames

# Now walk through and save all plots into "plots" folder
walk2(plotnames, flatten(all_plots), ~ggsave(filename = .x, 
                                             path = here::here("plots"),
                                             plot = .y, 
                                             height = 3, 
                                             width = 4,
                                             units = "in",
                                             dpi = 300))

# Overall F-tests for modeled data ----
# This is new, but folks want the overall F test of the output
    # from Woodstock for years 20, 50, 100
# This ignores assumptions but relaxes assumption of constant variance
# Need F statistic, df, p-value
# Testing
test1 = oneway.test(CARBac ~ Treatment, data = respdat_years)
test1$statistic
test1$parameter
test1$p.value

# Ultimately I guess will save df in separate columns
data.frame("F" = test1$statistic,
           "numerator_df" = test1$parameter[1],
           "denominator_df" = round(test1$parameter[2], digits = 1),
           "p-value" = test1$p.value)
           
# Ultimately need to get rid of scientific notation here
options(scipen = 999)

# Function that takes data and builds formula
oneway_fun = function(data, yvar) {
    fit = oneway.test(as.formula(paste0(yvar, "~ Treatment")), data = data)
    data.frame("F" = fit$statistic,
               "numerator_df" = fit$parameter[1],
               "denominator_df" = round(fit$parameter[2], digits = 1),
               "p-value" = fit$p.value)
}

# Test function
oneway_fun(data = year_split[[1]], yvar = resps[[1]])

# Do tests
all_tests = map_dfr(year_split, 
                ~map_dfr(resps, function(yvar) {
                    oneway_fun(data = .x, yvar = yvar)
                },
                .id = "Response"),
                .id = "Year")

# Save the data.frame into "results"
# write.csv(all_tests,
#           file = here::here("results", "2020-11_ESRF_ANOVA_unequal_variances.csv"),
#           row.names = FALSE)

# Power analysis ----

# Focus on years 20, 50, 100
sumdat_years = filter(sumdat, Year %in% c(20, 50, 100) )

# Pad year with 0 for naming saved plots
sumdat_years$Year = sprintf("%03d", sumdat_years$Year)

# Supposedly we want to try smaller sample sizes,
    # so I will do 4-9 watersheds per treatment
    # plus the current unbalanced selection
# I can do this by looping through and changing the n column
# I will use 10 as placeholder for the current selected sample sizes
all_n = map_dfr(set_names(4:10), ~if(.x == 10) {
    sumdat_years
    } else {
    sumdat_years$n = .x
    sumdat_years
    }, .id = "number")

# Split this by number, response, treatment
num_split = split(all_n, list(all_n$number, all_n$response, all_n$Year))

# Get function for power analysis
# Right now only calculating power and not the
    # (as important) effect sizes so this function is slightly
    # simpler than other ones I've used in this project

# I considered using a single argument as the dataset but
    # ended up sticking with this
anova_fun = function(groups, n, means, vars) {
    trt = rep(groups, times = n)
    y = unlist( pmap(list(n, means, vars),
                     function(n, mean, var) {
                         rnorm(n = n, mean = mean, sd = sqrt(var) )
                     } ) )
    

    modelw = oneway.test(y ~ trt)
    
    results = data.frame(p = modelw$p.value)
    results
}

# Test function ----
# It looks fine
# set.seed(16)
# anova_fun(groups = num_split[[1]]$Treatment,
#           n = num_split[[1]]$n,
#           means = num_split[[1]]$mean,
#           vars = num_split[[1]]$variance)
# 
# 
# set.seed(16)
# trt = rep(num_split[[1]]$Treatment, times = num_split[[1]]$n)
# y = unlist( pmap(list(num_split[[1]]$n, num_split[[1]]$mean, num_split[[1]]$variance),
#                  function(n, mean, var) {
#                      rnorm(n = n, mean = mean, sd = sqrt(var) )
#                  } ) )
# oneway.test(y ~ trt)
# 
# set.seed(16)
# rnorm(n = 9, mean = 9.709633, sd = sqrt(7.786724) )
# rnorm(n = 10, mean = 6.133940, sd = sqrt(5.807498) )


# Run power analysis using 1000 simulations for each dataset
all_res = map_dfr(num_split, ~do.call("rbind", replicate(n = 1000, 
                                                    expr = anova_fun(groups = .x$Treatment,
                                                                     n = .x$n,
                                                                     means = .x$mean,
                                                                     vars = .x$variance),
                                                    simplify = FALSE)),
                  .id = "sample")

# Separate out names back into columns
all_res = separate(all_res, sample, into = c("n", "Response", "Year"))

# Whoops, I got all the p-values but didn't actually calculate power
# Round to 2 digits
power = all_res %>%
    group_by(n, Response, Year) %>%
    summarise(Power = round(mean(p < .05), digits = 2), .groups = "drop")

# Set factor levels  and labels for "n"
# Put things in order by response, year, n
power = power %>%
    mutate(n = factor(n, levels = 1:10, 
                         labels = c(1:9, "Complete sample") ) ) %>%
    arrange(Response, Year, n)

# Save this as results
# write.csv(power,
#           file = here::here("results", "2020-11_ESRF_estimated_power.csv"),
#           row.names = FALSE)
# Read back in if need again
# power = read.csv(here::here("results", "2020-11_ESRF_estimated_power.csv"))


# Graph power with lollipop charts ----

# 2020-11-05 Scott requested we get rid of samples sizes <6
power = filter(power, !n %in% c(4, 5) )

# This will involve splitting the power dataset for looping
# Add leading 0 before split by year
power$Year = sprintf("%03d", power$Year)
power_split = split(power, list(power$Response, power$Year) )

# Test plot
# Basing theme off example in ggalt (code on github)
ggplot(data = power_split[[1]], aes(x = Power, y = stringr::str_wrap(n, width = 8) ) ) +
    geom_lollipop(point.colour = "steelblue", point.size = 2, horizontal = TRUE) + 
    scale_x_continuous(expand = expansion(add = c(0, .02) ),                                                                                                       
                       breaks=seq(0, 1, by = 0.2), limits = c(0, 1) ) +
    labs(y = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.length.x = unit(0, "pt"),
          axis.text.y = element_text(color = "black", margin = margin(l = 0, r = 0) ),
          axis.text.x = element_text(color = "black"),
     
          # axis.line.y = element_line(size = 0.15)
          # axis.title.y = element_markdown(vjust = 10, 
          #                                 margin = unit(c(r = -10, 0, 0, 0), "pt")
          #                                 ) 
          )

# Test sizes when saving
# ggsave("test_lolli.png", path = here::here("plots"),
#        width = 3, height = 3, dpi = 300)


# Function for plotting
lollipop_fun = function(data) {
    ggplot(data = data, aes(x = Power, y = stringr::str_wrap(n, width = 8) ) ) +
        geom_lollipop(point.colour = "steelblue", point.size = 2, horizontal = TRUE) + 
        scale_x_continuous(expand = expansion(add = c(0, .02) ),                                                                                                       
                           breaks=seq(0, 1, by = 0.2), limits = c(0, 1) ) +
        labs(y = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.length.x = unit(0, "pt"),
              axis.text.y = element_text(color = "black", margin = margin(l = 0, r = 0) ),
              axis.text.x = element_text(color = "black") )
}


# Run through all datasets and plot
all_lolli = map(power_split, lollipop_fun)

# Will need to use list names for naming
stringr::str_sub("WIWA.100", start = -3) # extract year
stringr::str_sub("WIWA.100", end = -5) # extract response

# Save all plots with names based on response and year
iwalk(all_lolli, ~ggsave(filename = paste0("2020-11_ESRF_lollipop_", 
                                           stringr::str_sub(.y, end = -5), "_", "year", 
                                           stringr::str_sub(.y, start = -3), ".png"), 
                         path = here::here("plots"),
                         plot = .x, 
                         height = 3, 
                         width = 3,
                         units = "in",
                         dpi = 300))


# 2020-11-05 May need to keep years together in one plot
power_split2 = split(power, power$Response)

# Remember year is a character with leading 0 but will now
    # want to remove those if keep a legend

# Chose colors by getting 7 and taking 3, 5, 7 
colorspace::specplot(colorspace::sequential_hcl(n = 7,
                           palette = "Blues", rev = TRUE))
dput(colorspace::sequential_hcl(n = 7,
                                palette = "Blues", rev = TRUE))
# col2rgb(c("#ACCCE4", "#5087C1", "#273871"))
# [,1] [,2] [,3]
# red    172   80   39
# green  204  135   56
# blue   228  193  113

# Make labels for facets
fac_labels = c('020' = "20 years",
                '050' = "50 years",
                '100' = "100 years")

# Use facet labels on right instead of legend
ggplot(data = power_split2[[1]], aes(x = Power, y = stringr::str_wrap(n, width = 8) )  ) +
    geom_segment(aes(x = 0, xend = Power, y = stringr::str_wrap(n, width = 8), yend = stringr::str_wrap(n, width = 8))) +
    geom_point(aes(color = factor(as.numeric(Year))) , size = 2) + 
    facet_wrap(~Year, ncol = 1, strip.position = "right", 
               labeller = labeller(Year = fac_labels) ) +
    scale_x_continuous(expand = expansion(add = c(0, .02) ),                                                                                                       
                       breaks=seq(0, 1, by = 0.2), limits = c(0, 1) ) +
    labs(y = NULL) +
    scale_color_manual(values = c("#ACCCE4", "#5087C1", "#273871"),
                       guide = "none") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks.length.x = unit(0, "pt"),
          axis.text.y = element_text(color = "black", margin = margin(l = 0, r = 0) ),
          axis.text.x = element_text(color = "black"),
          panel.spacing = unit(0, "lines"),
          #plot.margin = margin(0, 0, 0, 0)
    )

# Test sizes when saving
# ggsave("test_lolli.png", path = here::here("plots"),
#        width = 3, height = 4, dpi = 300)

# Function for plotting
lollipop_fun2 = function(data) {
    ggplot(data = data, aes(x = Power, y = stringr::str_wrap(n, width = 8) ) ) +
        geom_segment(aes(x = 0, xend = Power, y = stringr::str_wrap(n, width = 8), yend = stringr::str_wrap(n, width = 8))) +
        geom_point(aes(color = factor(as.numeric(Year))) , size = 2) + 
        facet_wrap(~Year, ncol = 1, strip.position = "right", 
                   labeller = labeller(Year = c('020' = "20 years",
                                                '050' = "50 years",
                                                '100' = "100 years") ) ) +
        scale_x_continuous(expand = expansion(add = c(0, .02) ),                                                                                                       
                           breaks=seq(0, 1, by = 0.2), limits = c(0, 1) ) +
        labs(y = NULL) +
        scale_color_manual(values = c("#ACCCE4", "#5087C1", "#273871"),
                           guide = "none") +
        theme_minimal(base_size = 12) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.length.x = unit(0, "pt"),
              axis.text.y = element_text(color = "black", margin = margin(l = 0, r = 0) ),
              axis.text.x = element_text(color = "black"),
              panel.spacing = unit(0, "lines"),
              #plot.margin = margin(0, 0, 0, 0) 
              )
}

    
# Run through all datasets and plot
all_lolli2 = map(power_split2, lollipop_fun2)

# Save all plots with names based on response and year
iwalk(all_lolli2, ~ggsave(filename = paste0("2020-11_ESRF_lollipop_", 
                                           .y, "_", "allyears", ".png"), 
                         path = here::here("plots"),
                         plot = .x, 
                         height = 4, 
                         width = 3,
                         units = "in",
                         dpi = 300))

# Using dodging instead of facets (not needed)
# Turns out dodging doesn't work correctly for segments
# power$nstring = factor(stringr::str_wrap(power$n, width = 8))
# power_split2 = split(power, power$Response)
# 
# ggplot(data = power_split2[[1]], aes(x = Power, y = nstring)  ) +
#     geom_blank() + # to get discrete scale to work
#     geom_segment(aes(x = 0, xend = Power, 
#                      group = Year,
#                      y = case_when(Year == "050" ~ as.numeric(nstring),
#                                    Year == "020" ~ as.numeric(nstring) + .3,
#                                    Year == "100" ~ as.numeric(nstring) - .3), 
#                      yend = case_when(Year == "050" ~ as.numeric(nstring),
#                                       Year == "020" ~ as.numeric(nstring) + .3,
#                                       Year == "100" ~ as.numeric(nstring) - .3) ) ) +
#     geom_point(aes(color = factor(as.numeric(Year), levels = c(100, 50, 20))) , size = 2,
#                position = position_dodge(width = .9) ) + 
#     scale_x_continuous(expand = expansion(add = c(0, .02) ),                                                                                                       
#                        breaks=seq(0, 1, by = 0.2), limits = c(0, 1) ) +
#     labs(y = NULL) +
#     scale_color_manual(values = rev(c("#7FABD3", "#3573B9", "#273871")),
#                        name = "Year", guide = guide_legend(direction = "horizontal",
#                                                            reverse = TRUE)) +
#     theme_minimal(base_size = 12) +
#     theme(panel.grid.major.y = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.ticks.length.x = unit(0, "pt"),
#           axis.text.y = element_text(color = "black", margin = margin(l = 0, r = 0) ),
#           axis.text.x = element_text(color = "black"),
#           legend.position = "top",
#           legend.justification = "right",
#           legend.margin = margin(0, 0, 0, 0),
#           panel.spacing = unit(0, "lines"),
#           legend.box.margin = margin(-10,-10,-10, r = 0),
#           legend.spacing.x = unit(0, unit = "pt"),
#           legend.title = element_text(size = 11)
#           
#           # axis.line.y = element_line(size = 0.15)
#           # axis.title.y = element_markdown(vjust = 10, 
#           #                                 margin = unit(c(r = -10, 0, 0, 0), "pt")
#           #                                 ) 
#     )


# Test sizes when saving
# ggsave("test_lolli2.png", path = here::here("plots"),
#        width = 3, height = 4, dpi = 300)