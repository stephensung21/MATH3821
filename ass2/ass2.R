library(ggplot2)

# Read Data
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
