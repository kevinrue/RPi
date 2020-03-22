source("temperature-functions.R")

temperature_input_table <- read_temperature_file("examples/temperature.txt")

temperature_work_table <- temperature_input_table %>%
    drop_pre_measurements(min_increase=0.5) %>%
    drop_failed_measurements()

# ggplot(temperature_work_table) +
#     geom_point(aes(time, temperature))

temperature_work_table <- temperature_work_table %>%
    mutate(
        dT = c(NA, diff(temperature, lag = 2), NA),
        dt = c(NA, diff(time, lag = 2), NA),
        dT_dt = dT / dt) %>%
    subset(!is.na(dT_dt)) %>%
    reset_time()

ggplot(temperature_work_table) +
    geom_point(aes(time, temperature)) +
    geom_segment(
        aes(x = time, y = temperature, xend = time + 1, yend = temperature + dT_dt),
        arrow = arrow(length = unit(0.03, "npc"))
        )

ggplot(temperature_work_table) +
    geom_point(aes(time, dT_dt)) +
    coord_cartesian(ylim = c(0, max(temperature_work_table$dT_dt, na.rm = TRUE)))

fit <- lm(log(dT_dt) ~ time, temperature_work_table)
# abs(MASS::stdres(fit)) < 1
temperature_work_table <- temperature_work_table[abs(MASS::stdres(fit)) < 1, , drop=FALSE] %>% reset_time()
fit <- lm(log(dT_dt) ~ time, temperature_work_table)
# plot(fit)
# summary(fit)

temperature_work_table <- temperature_work_table %>%
    mutate(predict = exp(predict(fit, list(time=time))))

ggplot(temperature_work_table) +
    geom_point(aes(time, dT_dt)) +
    geom_line(aes(time, predict), temperature_work_table) +
    coord_cartesian(ylim = c(0, max(temperature_work_table$dT_dt, na.rm = TRUE)))

T_0 <- temperature_work_table %>% pull(temperature) %>% head(1)

pred_table <- tibble(time = seq(0, 10E3))
pred_table <- pred_table %>% mutate(
    dT_dt = exp(predict(fit, list(time=time))),
    cumsum = cumsum(dT_dt),
    temperature = T_0 + cumsum
)

T_final <- pred_table %>% pull(temperature) %>% tail(1)

ggplot(subset(pred_table, dT_dt > 0.0001 & time < 20*60)) +
    geom_line(aes(time, dT_dt)) +
    coord_cartesian(ylim = c(0.0001, max(pred_table$dT_dt, na.rm = TRUE)))

ggplot() +
    geom_hline(yintercept = T_final, color = "red") +
    geom_point(aes(time, temperature), temperature_work_table) +
    geom_line(
        aes(time, temperature), subset(pred_table, dT_dt > 0.0001 & time < 20*60),
        color = "blue", linetype = "dashed") +
    coord_cartesian(ylim = c(0.0001, 40))


cat(sprintf("Predicted temperature (Celsius): %.1f", T_final))
