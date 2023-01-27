# # Part B, 15/24 -- 9) 5 pts

# install.packages('dplyr')
# install.packages('plotly')
# install.packages('ggplot2')

library(dplyr)
library(plotly)
library(ggplot2)

df = readRDS('data.RDS')

Q9 = df %>% 
  group_by(country_name) %>% 
  summarise(mean_polyarchy= mean(v2x_polyarchy)) %>% 
  arrange(desc(mean_polyarchy))

# # Part B, 16/24 -- 10) 5 pts

democracy_index = select(df, -c('country_name', 'year', 'v2x_regime'))
democracy_index = round(data.frame(rowMeans(democracy_index)), digit=3)

Q10 = data.frame(df, democracy_index)
Q10 = rename(Q10, 'democracy_index'='rowMeans.democracy_index.')

# # Part B, 17/24 -- 11) 10 pts

Q11 = Q10 %>%
  group_by(country_name) %>%
  mutate(democracy_rank=rank(desc(democracy_index)))

# # Part B, 19/24 -- 12) 5 pts

Q12 = Q11 %>% mutate(Year_Group=case_when(year < 2016 ~ '2010-2015', TRUE~'2016-2021'))

# # Part B, 20/24 -- 13) 5 pts
Q13 = Q12
Q13$v2x_regime[Q13$v2x_regime == 0] = 'Closed autocracy'
Q13$v2x_regime[Q13$v2x_regime == 1] = 'Electoral autocracy'
Q13$v2x_regime[Q13$v2x_regime == 2] = 'Electoral democracy'
Q13$v2x_regime[Q13$v2x_regime == 3] = 'Liberal democracy'

# # Part B, 21/24 -- 14) 10 pts

discus = Q13$v2xcl_disc
academ = Q13$v2xca_academ
regime = Q13$v2x_regime

country = Q13$country_name
year = Q13$year

text=paste('country:', country, '\nyear:', year)

Q14 = ggplot(data=Q13) +
  geom_point(mapping = aes(x=discus,
                           y=academ,
                           colour=regime,
                           text=text)) +
  facet_wrap(~ Year_Group) +
  ggtitle('Freedom to Discuss vs Academic freedom by Regime for year periods') +
  xlab('Freedom to discuss') +
  ylab('Academic Freedom') +
  labs(colour='Regime type')

# # Part B, 22/24 -- 15) 5 pts

Q15 = ggplotly(Q14)
