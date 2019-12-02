library(tidyverse)
library(magrittr)
library(broom)
library(cowplot)

data(nzcathist)

nz <- nzcathist %>%
    as_tibble()

nz %>% glimpse()

data(teengamb, package = "faraway")
tg <- teengamb %>% as_tibble()
lmod <- lm(gamble ~ ., tg)
lm_aug <- lmod %>% augment()

diag_plot <- function(data, x, y) {
    data %>%
        ggplot(aes(x = {{x}}, y = {{y}})) +
        geom_point() +
        geom_hline(yintercept = 0, lty = 2, alpha = 0.5)
}

qq_plot <- function(data, choice, title) {
    data %>%
        ggplot(aes(sample = {{choice}})) +
        stat_qq() +
        stat_qq_line() +
        ggtitle(title)
}

large_lev_cutoff <- function(model) {
    # get number of predictors
    model %>% tidy() %>% nrow() - 1 -> pred_count

    # get amount of data
    model %>% augment() %>% nrow() -> data_count

    # print message
    cat(paste0("Returning Leverage Points Over ",
               round(2 * pred_count / data_count, 2), "\n"))

    # get metric
    return(2 * pred_count / data_count)
}

get_large_lev_points <- function(model) {
    # get cutoff
    model %>% large_lev_cutoff() -> cutoff_point

    # return hat values
    model %>%
        augment() %>%
        mutate(index = row_number()) %>% 
        filter(.hat >= cutoff_point) %>%
        select(index, .hat, everything()) %>%
        arrange(desc(.hat))
}

gen_leverage_plots <- function(model) {
    model %>%
        augment() -> lm_aug

    lm_aug$.hat %>%
        gghalfnorm::gghalfnorm() -> p1

    lm_aug %>%
        qq_plot(.std.resid, title = "Standardized Residuals") -> p2

    cowplot::plot_grid(p1, p2, ncol = 1)
}

get_adj_outliers <- function(lmod,
                      base_p_value = 0.05,
                      correction_strength = NA,
                      top_vals = 3,
                      true_only = FALSE) {
    # get studentized resids
    st_res <- rstudent(lmod)

    # set correction strength
    if (is.na(correction_strength)) {
        correction_strength <- lmod %>%
            augment() %>%
            nrow()
    } else {
        correction_strength
    }

    # compute bonferroni
    dof <- lmod %>%
        glance() %>%
        pull(df.residual)

    base_p_value <- base_p_value

    # compute conferroni
    bf_val <- qt(p = base_p_value / correction_strength,
                 df = dof)

    # get top_n absolute value studentized residuals
    top_n_vals <- st_res %>%
        enframe() %>%
        top_n(abs(value), n = top_vals) %>%
        add_row(name = "Bonferroni Correction Threshold",
                value = bf_val) %>%
        rowwise() %>%
        mutate(over_thresh = ifelse(abs(value) > abs(bf_val),
                                    TRUE,
                                    FALSE))

    # get outlier values
    top_n_vals %>%
        pluck(1) %>%
        head(-1) %>%
        map_df(., ~ lmod %>%
            augment() %>%
            slice(as.integer(.x)) %>%
            mutate(name = .x)) -> outlier_values

    if (true_only) {
        outlier_values %<>%
            filter(over_thresh)
    }
    
    outlier_values %>%
        left_join(top_n_vals, by = "name") %>%
        select("Index" = name, over_thresh, value, everything())
}

partial_regression_plot <- function(data, response, var) {
    # get vars that aren't our var
    vars <- colnames(data) %>%
        str_remove(c({{response}}, {{var}})) %>%
        .[. != ""] %>%
        paste(collapse = " + ")

    formula_1 <- paste0(response, " ~ ", vars) %>% as.formula()
    formula_2 <- paste0(var, " ~ ", vars) %>% as.formula()

    # fit lmod
    lmod_1 <- lm(formula_1, data)
    lmod_2 <- lm(formula_2, data)

    # get residuals
    response_resid <- lmod_1 %>%
        augment() %>%
        select(.resid) %>%
        flatten_dbl()

    var_resid <- lmod_2 %>%
        augment() %>%
        select(.resid) %>%
        flatten_dbl()

    resids <- tibble("var" = var_resid,
                     "response" = response_resid)
    resids %>%
        ggplot(aes(x = var, y = response)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE,
                    lty = 2, color = "mediumpurple", alpha = 0.3) +
        ggtitle(var)
}

gen_all_partial <- function(data, response) {
    plots <- data %>%
        names() %>%
        .[!. %in% response] %>%
        map(., ~ partial_regression_plot(data, response = response, var = .x))

    plot_grid(plotlist = plots)
}

partial_residual_plot <- function(model, predictor) {
    # get terms
    terms <- predict(model, type = "terms") %>%
        as_tibble() %>%
        select({{predictor}}) %>%
        flatten_dbl()

    model %>%
        augment() %>%
        mutate(partial_resid = terms + .resid) %>%
        ggplot(aes(x = {{predictor}}, y = partial_resid)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE,
                    color = "mediumpurple", lty = 2) +
        ylab("Partial Residuals")
}

gen_all_partial_resids <- function(model) {
    model %>%
        tidy() %>%
        .[2:nrow(.), 1] %>%
        flatten_chr() %>%
        map(sym) %>%
        map(., ~ partial_residual_plot(lmod, !!.x)) -> pr_plots

    plot_grid(plotlist = pr_plots, ncol = 2)
}
