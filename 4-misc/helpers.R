qhist <- function(data, var) {
  ggplot(data, aes({{ var }})) +
    geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))
}

qscatter <- function(data, var) {
  ggplot(data, aes({{ var }}, disability)) +
    geom_point() +
    geom_smooth()
}

qboxplot <- function(data, var) {
  ggplot(data, aes({{ var }}, disability)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.3)
}

residvfit <- function(fit) {
  ggplot(fit, aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 1,
      color = "red"
    ) +
    geom_smooth(
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      color = "blue"
    )
}

residvlev <- function(mod, fit) {
  p <- length(coef(mod))
  n <- nrow(data)
  
  ggplot(fit, mapping = aes(x = .hat, y = .std.resid)) +
    geom_point(mapping = aes(size = .cooksd)) +
    geom_hline(yintercept = c(-2, 2), linetype = "dashed") +
    geom_vline(xintercept = 2 * (p / n), linetype = "dashed") +
    ggrepel::geom_label_repel(mapping = aes(label = .rownames),
                              color = "grey", fill = "white", size = 2, box.padding =  0.4, 
                              label.padding = 0.1) +
    labs(x = "Leverage", y = "Standardized residuals")
}

univar <- function(data, name) {
  # fit model
  form <- formula(str_c("disability ~ ", name))
  mod <- lm(form, data)
  
  # print out p value
  mod |> tidy(conf.int = TRUE, conf.level = 0.95) |> 
    mutate(meets_cutoff = p.value < 0.1) |> 
    print()
  
  fit <- augment(mod) |> 
    select(.fitted, .resid)
  
  # fitted vs residual plot
  print(residvfit(fit) +
    labs(x = "Fitted values", y = "Residuals"))
  
  # normality plot
  print(qhist(fit, .resid) +
    labs(x = ".resid"))
}




