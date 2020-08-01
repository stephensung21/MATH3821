library(ggplot2)
library(dplyr)
library(MASS)

### READ DATA
hitters.df<-read.table('hitter.txt', sep="\t")
colnames(hitters.df)<-c('name','bats','hits','home_runs','runs','runs_batted','walks','years','times_at_bat',
                        'career_hits','career_home_runs','career_runs','career_runs_batted','career_walks',
                        'league_86','div_86','team_86','pos_86','put_outs','assists','errors','salary',
                        'league_87','team_87')
head(hitters.df)

pitchers.df<-read.table('pitcher.txt', sep='\t')
colnames(pitchers.df)<-c('name','team_86','league_86','wins','losses','earn_run_average','games',
                         'innings_pitched','saves','years','career_wins','career_losses',
                         'career_earn_run_average','career_games','career_innings_pitched','career_saves',
                         'salary','league_87','team_87')
head(pitchers.df)


hist(hitters.df$salary, breaks=20)
summary(hitters.df$salary)

hist(pitchers.df$salary, breaks=20)
summary(pitchers.df$salary)

### HITTERS LM
hitters.lm<-lm(salary~.-name-team_86-team_87, data=hitters.df)
summary(hitters.lm)

#stepwise regression, no transformation
step.model <- stepAIC(hitters.lm, direction = 'both',trace=FALSE)
summary(step.model)

#box cox, transformation
bc <- boxcox(step.model, data=hitters.df)

#shows that a log transformation is good

#stepwise regression, with log transformation
hitters.lm2<-lm(log(salary)~.-name-team_86-team_87, data=hitters.df)
step.model2 <- stepAIC(hitters.lm2, direction = 'both',trace=FALSE)
summary(step.model2)

plot(step.model2)

#polynomial regression
hitters.lm3<-lm(salary~.-name-team_86-team_87+
                  poly(years,2)+poly(times_at_bat,2)+poly(career_hits,2)+poly(career_runs,2)+poly(career_runs_batted,2)+poly(career_walks,2),data=hitters.df)

summary(hitters.lm3)

#polynomial regression + log transform
hitters.lm4<-lm(log(salary)~.-name-team_86-team_87+
                  poly(years,2)+poly(times_at_bat,2)+poly(career_hits,2)+poly(career_runs,2)+poly(career_runs_batted,2)+poly(career_walks,2),data=hitters.df)
summary(hitters.lm4)

#stepwise regression
step.model4 <- stepAIC(hitters.lm4, direction = 'both',trace=FALSE)
summary(step.model4)
AIC(step.model4)
AIC(step.model2)
AIC(step.model)
AIC(hitters.lm4)

plot(step.model4)

# cubic
lm_more <- lm(log(salary)~bats+hits+walks+put_outs+poly(years, 2)+poly(career_runs, 2)+poly(career_runs_batted, 2) +poly(years,3)+, data=hitters.df)

test_3<-lm(log(salary)~.-name-team_86-team_87+
                  poly(years,2)+poly(times_at_bat,2)+poly(career_hits,2)+poly(career_runs,2)
           +poly(career_runs_batted,2)+poly(career_walks,2)+poly(years,3)+poly(times_at_bat,3)+poly(career_hits,3)+poly(career_runs,3)
           +poly(career_runs_batted,3)+poly(career_walks,3),data=hitters.df)

summary(test_3)
plot(test_3)

step.model_cube <- stepAIC(test_3, direction = 'both',trace=FALSE)
summary(step.model_cube)

anova(step.model4,step.model_cube)

par(mfrow = c(2,2))
plot(step.model_cube)

#getting rid of pvalue > 0.05
final_hit_lm <- lm(log(salary)~walks+career_home_runs+put_outs+years+I(years^2)+I(years^3)+I(times_at_bat^3)+career_hits+I(career_hits^2)+I(career_hits^3), data=hitters.df)

summary(final_hit_lm)
plot(final_hit_lm)


### PITCHERS LM
pitchers.lm<-lm(salary~.-name-team_86-team_87, data=pitchers.df)
summary(pitchers.lm)

plot(pitchers.lm)

#stepwise
step.modelp<-stepAIC(pitchers.lm, direction = 'both',trace=FALSE)
summary(step.modelp)

#boxcox
bcp<-boxcox(step.modelp,data=pitchers.df)

#pitchers lm with log transform
pitchers.lm2<-lm(log(salary)~.-name-team_86-team_87, data=pitchers.df)
summary(pitchers.lm2)

plot(pitchers.lm2)

#stepwise
step.modelp2<-stepAIC(pitchers.lm2, direction = 'both',trace=FALSE)
summary(step.modelp2)

plot(step.modelp2)


#interaction terms
pitchers.lm4<-lm(log(salary)~.-name-team_86-team_87+I(years^2)+I(career_wins^2)+I(career_losses^2)+I(career_losses^2)+I(career_innings_pitched^2), data=pitchers.df)
summary(pitchers.lm4)

plot(pitchers.lm4)

#stepwise
step.modelp3<-stepAIC(pitchers.lm4, direction = 'both',trace=FALSE)
plot(step.modelp3)
summary(step.modelp3)


#cube
pitchers.lm5<-lm(log(salary)~.-name-team_86-team_87+I(years^2)+I(career_wins^2)+I(career_losses^2)+I(career_losses^2)+I(career_innings_pitched^2)
                 +I(years^3)+I(career_wins^3)+I(career_losses^3)+I(career_losses^3)+I(career_innings_pitched^3), data=pitchers.df)
summary(pitchers.lm5)
plot(pitchers.lm5)

step.modelp4<-stepAIC(pitchers.lm5, direction = 'both',trace=FALSE)
summary(step.modelp4)
plot(step.modelp4)

#getting rid of pvalue > 0.05
final_pitch_lm <- lm(log(salary)~losses+earn_run_average+years+career_earn_run_average+career_innings_pitched+league_87+I(years^2)+I(career_innings_pitched^2)+I(years^3)+I(career_wins^3)+I(career_innings_pitched^3), data=pitchers.df)
summary(final_pitch_lm)



#-----------------------------------------------------------------------------------------------------
library(mgcv)
