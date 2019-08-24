# extract first 100 rows of the vehicles dataset
library(fueleconomy)
data(vehicles)
df <- vehicles[1:100, ]
str(df)

# demonstrate how GGally::ggpairs works for different types of variables
library(GGally)
quant_df <- df[, c("cyl", "hwy", "cty")]
ggpairs(quant_df)

mixed_df <- df[, c("fuel", "hwy")]
ggpairs(mixed_df)

cat_df <- df[, c("fuel", "make", "drive")]
ggpairs(cat_df)

# scaffold for my plot
library(gridExtra)
library(tidyverse)
grobs <- list()
idx <- 0
for (i in 1:ncol(cat_df)) {
    for (j in 1:ncol(cat_df)) {
        idx <- idx + 1
        
        # get feature names (note that i & j are reversed)
        x_feat <- names(cat_df)[j]
        y_feat <- names(cat_df)[i]
        
        if (i < j) {
            # empty canvas
            grobs[[idx]] <- ggplot() + theme_void()
        } else if (i == j) {
            # just the name of the variable
            label_df <- data.frame(x = -0, y = 0, label = x_feat)
            grobs[[idx]] <- ggplot(label_df, aes(x = x, y = y, label = label), 
                                   fontface = "bold", hjust = 0.5) +
                geom_text() +
                coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
                theme_void()
        }
        else {
            # 2-dimensional barplot
            grobs[[idx]] <- ggplot(cat_df, aes_string(x = x_feat)) + 
                geom_bar() +
                facet_grid(as.formula(paste(y_feat, "~ ."))) +
                theme(legend.position = "none", axis.title = element_blank())
        }
    }
}
grid.arrange(grobs = grobs, ncol = ncol(cat_df))

# as above, except we now have heatmaps for the part above the diagonal
grobs <- list()
idx <- 0
for (i in 1:ncol(cat_df)) {
    for (j in 1:ncol(cat_df)) {
        idx <- idx + 1
        
        # get feature names (note that i & j are reversed)
        x_feat <- names(cat_df)[j]
        y_feat <- names(cat_df)[i]
        
        if (i < j) {
            # frequency proportion heatmap
            # get frequency proportions
            freq_df <- cat_df %>% 
                group_by_at(c(x_feat, y_feat)) %>%
                summarize(proportion = n() / nrow(cat_df)) %>% 
                ungroup()
            
            # get all pairwise combinations of values
            temp_df <- expand.grid(unique(cat_df[[x_feat]]), 
                                   unique(cat_df[[y_feat]]))
            names(temp_df) <- c(x_feat, y_feat)
            
            # join to get frequency proportion
            temp_df <- temp_df %>%
                left_join(freq_df, by = c(setNames(x_feat, x_feat),
                                          setNames(y_feat, y_feat))) %>%
                replace_na(list(proportion = 0))
            
            grobs[[idx]] <- ggplot(temp_df, aes_string(x = x_feat, y = y_feat)) + 
                geom_tile(aes(fill = proportion)) +
                geom_text(aes(label = sprintf("%0.2f", round(proportion, 2)))) +
                scale_fill_gradient(low = "white", high = "#007acc") +
                theme(legend.position = "none", axis.title = element_blank())
        } else if (i == j) {
            # just the name of the variable
            label_df <- data.frame(x = -0, y = 0, label = x_feat)
            grobs[[idx]] <- ggplot(label_df, aes(x = x, y = y, label = label), 
                                   fontface = "bold", hjust = 0.5) +
                geom_text() +
                coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
                theme_void()
        }
        else {
            # 2-dimensional barplot
            grobs[[idx]] <- ggplot(cat_df, aes_string(x = x_feat)) + 
                geom_bar() +
                facet_grid(as.formula(paste(y_feat, "~ ."))) +
                theme(legend.position = "none", axis.title = element_blank())
        }
    }
}
grid.arrange(grobs = grobs, ncol = ncol(cat_df))

# as above, except we now have marginal barplots on the diagonal
grobs <- list()
idx <- 0
for (i in 1:ncol(cat_df)) {
    for (j in 1:ncol(cat_df)) {
        idx <- idx + 1
        
        # get feature names (note that i & j are reversed)
        x_feat <- names(cat_df)[j]
        y_feat <- names(cat_df)[i]
        
        if (i < j) {
            # frequency proportion heatmap
            # get frequency proportions
            freq_df <- cat_df %>% 
                group_by_at(c(x_feat, y_feat)) %>%
                summarize(proportion = n() / nrow(cat_df)) %>% 
                ungroup()
            
            # get all pairwise combinations of values
            temp_df <- expand.grid(unique(cat_df[[x_feat]]), 
                                   unique(cat_df[[y_feat]]))
            names(temp_df) <- c(x_feat, y_feat)
            
            # join to get frequency proportion
            temp_df <- temp_df %>%
                left_join(freq_df, by = c(setNames(x_feat, x_feat),
                                          setNames(y_feat, y_feat))) %>%
                replace_na(list(proportion = 0))
            
            grobs[[idx]] <- ggplot(temp_df, aes_string(x = x_feat, y = y_feat)) + 
                geom_tile(aes(fill = proportion)) +
                geom_text(aes(label = sprintf("%0.2f", round(proportion, 2)))) +
                scale_fill_gradient(low = "white", high = "#007acc") +
                theme(legend.position = "none", axis.title = element_blank())
        } else if (i == j) {
            # df for positioning the variable name
            label_df <- data.frame(x = 0.5 + length(unique(cat_df[[x_feat]])) / 2, 
                                   y = max(table(cat_df[[x_feat]])) / 2, label = x_feat)
            # marginal barplot with variable name on top
            grobs[[idx]] <- ggplot(cat_df, aes_string(x = x_feat)) +
                geom_bar() +
                geom_label(data = label_df, aes(x = x, y = y, label = label),
                           size = 5)
        }
        else {
            # 2-dimensional barplot
            grobs[[idx]] <- ggplot(cat_df, aes_string(x = x_feat)) + 
                geom_bar() +
                facet_grid(as.formula(paste(y_feat, "~ ."))) +
                theme(legend.position = "none", axis.title = element_blank())
        }
    }
}
grid.arrange(grobs = grobs, ncol = ncol(cat_df))

# final version: clean up some of the axes
theme_update(legend.position = "none", axis.title = element_blank())

grobs <- list()
idx <- 0
for (i in 1:ncol(cat_df)) {
    for (j in 1:ncol(cat_df)) {
        idx <- idx + 1
        
        # get feature names (note that i & j are reversed)
        x_feat <- names(cat_df)[j]
        y_feat <- names(cat_df)[i]
        
        if (i < j) {
            # frequency proportion heatmap
            # get frequency proportions
            freq_df <- cat_df %>% 
                group_by_at(c(x_feat, y_feat)) %>%
                summarize(proportion = n() / nrow(cat_df)) %>% 
                ungroup()
            
            # get all pairwise combinations of values
            temp_df <- expand.grid(unique(cat_df[[x_feat]]), 
                                   unique(cat_df[[y_feat]]))
            names(temp_df) <- c(x_feat, y_feat)
            
            # join to get frequency proportion
            temp_df <- temp_df %>%
                left_join(freq_df, by = c(setNames(x_feat, x_feat),
                                          setNames(y_feat, y_feat))) %>%
                replace_na(list(proportion = 0))
            
            grobs[[idx]] <- ggplot(temp_df, aes_string(x = x_feat, y = y_feat)) + 
                geom_tile(aes(fill = proportion)) +
                geom_text(aes(label = sprintf("%0.2f", round(proportion, 2)))) +
                scale_fill_gradient(low = "white", high = "#007acc") +
                theme(axis.ticks = element_blank(), axis.text = element_blank())
        } else if (i == j) {
            # df for positioning the variable name
            label_df <- data.frame(x = 0.5 + length(unique(cat_df[[x_feat]])) / 2, 
                                   y = max(table(cat_df[[x_feat]])) / 2, label = x_feat)
            # marginal barplot with variable name on top
            grobs[[idx]] <- ggplot(cat_df, aes_string(x = x_feat)) +
                geom_bar() +
                geom_label(data = label_df, aes(x = x, y = y, label = label),
                           size = 5)
        }
        else {
            # 2-dimensional barplot
            grobs[[idx]] <- ggplot(cat_df, aes_string(x = x_feat)) + 
                geom_bar() +
                facet_grid(as.formula(paste(y_feat, "~ ."))) +
                theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
        }
    }
}
grid.arrange(grobs = grobs, ncol = ncol(cat_df))