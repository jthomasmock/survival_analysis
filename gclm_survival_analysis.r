library(tidyverse)
library(readxl)
library(survival)
library(ggfortify)
library(survminer)

# read in the raw data
raw_df <- read_xlsx("surv_data.xlsx")

# take a peek
head(raw_df)

# filter only the relevant genotypes and exclude any ages miscoded 
clean_df <- raw_df %>% 
    filter(gt %in% c("wt", "ko") & real_age >= 0) %>%
    select(sex, real_age, gt, real_age, censor) %>% 
    mutate(sex = factor(sex, levels = c(0, 1), labels = c("MALE", "FEMALE"), ordered = T),
           gt = factor(gt, levels = c("wt","ko"), labels = c("WT", "KO"), ordered = T)) %>% 
    na.omit()

clean_df %>% group_by(sex, gt) %>% count()

# run the KM fit
fit <- survfit(Surv(real_age, censor) ~ gt + sex, data = clean_df)

# check the fit
fit

# tidy the fit for graphing
tidy_fit <- broom::tidy(fit) %>% 
    separate(strata, c("gt", "sex"), sep = ",")  %>% 
    mutate(gt = str_remove(gt, "gt="),
           sex = str_trim(str_remove(sex, "sex="))) %>% 
    mutate(sex = factor(sex, levels = c("MALE", "FEMALE"), ordered = T),
           gt = factor(gt, levels = c("WT","KO"), labels = c("GCLM+/+", "GCLM-/-"), ordered = T))
tidy_fit %>% str()

# what about within a single sex?
fit_male <- survfit(Surv(real_age, censor) ~ gt, data = filter(clean_df, sex == "MALE"))
fit_female <- survfit(Surv(real_age, censor) ~ gt, data = filter(clean_df, sex == "FEMALE"))

# chi-square for GT differences within a sex
# Male is p < 0.0001, female is = 0.05 *** BUT this isn't pairwise ***

survdiff(Surv(real_age, censor) ~ gt, data = filter(clean_df, sex == "MALE"))
survdiff(Surv(real_age, censor) ~ gt, data = filter(clean_df, sex == "FEMALE"))

# pairwise testing?
pairwise_df <- clean_df %>% 
    na.omit() %>% 
    unite("pair_factor", c("sex", "gt")) %>% 
    mutate(pair_factor = factor(pair_factor))
pairwise_df

# output is the same with combined factors
pairwise_survdiff(Surv(real_age, censor) ~ pair_factor, data = pairwise_df)

# as with with classical formula
pairwise_survdiff(Surv(real_age, censor) ~ sex + gt, data = clean_df)


# generate median line for final plot
median_line <- tribble(
    ~x, ~y, ~xend, ~yend, ~sex, 
    0, 0.5, 25.2, 0.5, "MALE",
    0, 0.5, 23.3, 0.5, "FEMALE"
) %>% mutate(sex = factor(sex, levels = c("MALE", "FEMALE")))


# theme elements for final plot
th <- theme(legend.title = element_blank(),
            axis.title = element_text(size = 20),
            legend.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 12),
            strip.text = element_text(face = "bold", size = 12))

# generate plot from tidy survival output
(km_plot_tidy <- ggplot(tidy_fit, aes(time, estimate)) +
        geom_segment(data = median_line, 
                     aes(x = x, y = y, xend = xend, yend = yend), 
                     linetype = "dotted", size = 1) +  
        geom_step(aes(group = gt, color = gt), size = 1) +
        #geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = gt), alpha = 0.3) +
        facet_grid(~sex) +
        scale_color_manual(values = c("black", "red")) +
        scale_linetype_manual(values = c("solid", "dashed")) +
        
        theme_bw() +
        theme(legend.position = c(0.07, 0.15),
              legend.title = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 12),
              strip.text = element_text(size = 12, face = "bold")) +
        labs(x = "\nAGE (MONTHS)",
             y = "K-M SURVIVAL PROBABILITY\n"))

# save the final plot
ggsave("tidy_surv.png", km_plot_tidy, height = 5, width = 8.5, units = "in", dpi = 1000)
