# DHS Bangladesh exploration

library(here)

source(here("child-mortality-dhs/R", "0-config.R"))


## Load data
monthly_time_series <- readRDS(file = here("data/final", "monthly_time_series"))

monthly_time_series_arm <- (monthly_time_series %>%
                              mutate(Arm = replace(Region, Flooded == 1, "Flooded")) %>%
                              group_by(Birth_Date_Month_CMC, Arm) %>%
                              summarise(Flooded = first(Flooded),
                                        Post_2004 = first(Post_2004),
                                        Post_2007 = first(Post_2007),
                                        Number_Of_Birth = sum(Number_Of_Birth, na.rm = T),
                                        Number_Of_Dead_Birth = sum(Number_Of_Dead_Birth, na.rm = T)) %>%
                              group_by(Arm) %>%
                              mutate(Number_Of_Birth_QS = lag(Number_Of_Birth) + Number_Of_Birth + lead(Number_Of_Birth),
                                     Number_Of_Dead_Birth_QS = lag(Number_Of_Dead_Birth) + Number_Of_Dead_Birth + lead(Number_Of_Dead_Birth),
                                     Infant_Mortality = Number_Of_Dead_Birth/Number_Of_Birth,
                                     Infant_Mortality_Quarterly_Smoothed = Number_Of_Dead_Birth_QS/Number_Of_Birth_QS))

mydata <- (monthly_time_series_arm
           %>% filter(Birth_Date_Month_CMC %in% c((1255-7):(1255+5+6))) # Restrict to 2004
           # %>% filter(Birth_Date_Month_CMC %in% c((1291-6):(1291+5))) # Restrict to 2007
           %>% ungroup()
           %>% mutate(treated = as.numeric(as.character(Flooded)),
                      post = Post_2004,
                      treatedpost = treated*post)
           # %>% filter(Post_2007 == 0)
           %>% filter(!is.na(Infant_Mortality_Quarterly_Smoothed)) # because of lead and lag
           )

y <- gsynth(Infant_Mortality_Quarterly_Smoothed ~ treatedpost, 
            data = mydata,  EM = F, index = c("Arm","Birth_Date_Month_CMC"), 
            inference = "parametric", se = TRUE,
            nboots = 2,  r = c(0, 5), CV = TRUE, force = "two-way", parallel = FALSE)
            
            
y1 <- round(data.frame(y$est.avg),2)
y1

plot(y)
plot(y, type = "counterfactual", raw = "all")
