# how many domains visited in one session
groupedsessions <- 
  ezproxy %>% group_by(session,campus) %>% summarize(dom = length(unique(domain)))
summary(groupedsessions$dom)
qplot(groupedsessions$dom, main = "Distribution of number of domains visited per session", xlab = "Number of domains per session", ylab = "Count")

tukey <- TukeyHSD(aov(dom~campus,groupedsessions))
tukey
par(mar=c(6,7,3,7))
plot(tukey, las = 1)

pom <- ezproxy %>% filter(campus == "pom") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
cmc <- ezproxy %>% filter(campus == "cmc") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
cgu <- ezproxy %>% filter(campus == "cgu") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
cuc <- ezproxy %>% filter(campus == "cuc") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
scr <- ezproxy %>% filter(campus == "scr") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
pit <- ezproxy %>% filter(campus == "pit") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
hmc <- ezproxy %>% filter(campus == "hmc") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
kec <- ezproxy %>% filter(campus == "kec") %>% group_by(session) %>% summarize(dom = length(unique(domain)))
kgi <- ezproxy %>% filter(campus == "kgi") %>% group_by(session) %>% summarize(dom = length(unique(domain)))


# frequency of each domain visited
freq <- head(sort(table(ezproxy$domain),decreasing=T), 10)

library(ggplot2)
top_dom <- ezproxy %>% filter(domain %in% names(freq))
by_freq <- top_dom %>% select(campus, domain) %>% group_by(campus, domain) %>% summarize(counts = n()) %>% ggplot(aes(x = reorder(domain,-counts), y = counts, fill = campus)) + geom_bar(stat='identity') + ggtitle("Top 10 frequent domains vs campus") + xlab("Domain") + ylab("Count")
by_freq + theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_cgu <- head(sort(table(subset(ezproxy, campus=="cgu")$domain),decreasing=T),5)
top_cmc <- head(sort(table(subset(ezproxy, campus=="cmc")$domain),decreasing=T),5)
top_cuc <- head(sort(table(subset(ezproxy, campus=="cuc")$domain),decreasing=T),5)
top_hmc <- head(sort(table(subset(ezproxy, campus=="hmc")$domain),decreasing=T),5)
top_kec <- head(sort(table(subset(ezproxy, campus=="kec")$domain),decreasing=T),5)
top_kgi <- head(sort(table(subset(ezproxy, campus=="kgi")$domain),decreasing=T),5)
top_pit <- head(sort(table(subset(ezproxy, campus=="pit")$domain),decreasing=T),5)
top_pom <- head(sort(table(subset(ezproxy, campus=="pom")$domain),decreasing=T),5)
top_scr <- head(sort(table(subset(ezproxy, campus=="scr")$domain),decreasing=T),5)

by_campus <- unique(c(names(top_cgu),names(top_cmc)))

top_dom %>% filter(campus!="kec") %>% filter(domain %in% by_campus) %>% select(campus, domain) %>% 
  group_by(campus, domain) %>% summarize(counts = n()) %>% ggplot(aes(x = campus, y = counts, fill = domain)) + geom_bar(stat='identity') + ggtitle("Campus vs top 5 frequent domains") + xlab("Campus") + ylab("Counts")

# domain vs duration
clean_dur <- ezproxy %>% filter(session_duration_mins<24*60) %>% filter(session_duration_mins>122)
clean_dur$session_duration_mins[is.na(clean_dur$session_duration_mins)] <- 0
groupeddomains <- 
  clean_dur %>% group_by(domain) %>% summarize(duration = sum(session_duration_mins))

dur <- head(groupeddomains[order(groupeddomains$duration, decreasing = TRUE),], 10)
by_dur <- clean_dur %>% filter(domain %in% dur$domain) %>% select(campus, domain, session_duration_mins) %>% group_by(campus, domain) %>% summarize(duration_mins = sum(session_duration_mins)) %>% ggplot(aes(x = reorder(domain,-duration_mins), y = duration_mins, fill = campus)) + geom_bar(stat='identity') + ggtitle("Top 10 domains by duration vs campus") + xlab("Domain") + ylab("Duration sum in minutes")
by_dur + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# by patrons
patron <- preprocess_patron_data()

newlevel <- c("current faculty/staff", "current faculty/staff",
              "alumni", "community and family",
              "cst student", "current faculty/staff",
              "retired faculty/staff", "current faculty/staff",
              "graduate student", "graduate student", 
              "retired faculty/staff", "seniors",
              "current faculty/staff", "community and family",
              "undergraduate", "visiting scholar")

p <- patron
levels(p$patronType) <- newlevel

p %>% filter(domain %in% names(freq)) %>% select(patronType, domain) %>% group_by(patronType, domain) %>% summarize(counts = n()) %>% ggplot(aes(x = reorder(domain,-counts), y = counts, fill = patronType)) + geom_bar(stat='identity') + ggtitle("Top 10 frequent domains vs patron") + xlab("Domain") + ylab("Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

clean_dur2 <- p %>% filter(session_duration_mins<24*60) %>% filter(session_duration_mins>122)
clean_dur2$session_duration_mins[is.na(clean_dur2$session_duration_mins)] <- 0
groupeddomains2 <- 
  clean_dur2 %>% group_by(domain) %>% summarize(duration = sum(session_duration_mins))

dur2 <- head(groupeddomains2[order(groupeddomains2$duration, decreasing = TRUE),], 10)
by_dur2 <- clean_dur2 %>% filter(domain %in% dur2$domain) %>% select(patronType, domain, session_duration_mins) %>% group_by(patronType, domain) %>% summarize(duration_mins = sum(session_duration_mins)) %>% ggplot(aes(x = reorder(domain,-duration_mins), y = duration_mins, fill = patronType)) + geom_bar(stat='identity') + ggtitle("Top 10 domain by duration vs patronType") + xlab("Domain") + ylab("Duration sum in mins")
by_dur2 + theme(axis.text.x = element_text(angle = 45, hjust = 1))

patron_session_duration <- clean_dur2 %>% group_by(patronType) %>% summarize(avg_session_duration = mean(session_duration_mins))
patron_sessdur_barplot <- ggplot(patron_session_duration, aes(x = reorder(patronType,-avg_session_duration), y = avg_session_duration)) + geom_bar(stat = 'identity') + labs(x = "Patron type", y = "Mean session duration", title = "Patron type mean session durations for 2016") + theme(plot.title = element_text(hjust = 0.5, ), axis.text = element_text(size = 4))
patron_sessdur_barplot + theme(axis.text=element_text(angle = 45,hjust = 1,size=9))

ezproxy %>% filter(campus != "kec") %>%
  select(campus, session, domain) %>% 
  group_by(session, campus) %>%
  summarize(dom = length(unique(domain)), count = n()) %>%
  ggplot(aes(x = dom, y = count)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~ campus, nrow = 2) + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  labs(x = 'Number of domains per session', 
       y = 'Count',
       title = 'Distribution of number of domains per session by campus')

# top by patron type

top_cfs <- head(sort(table(subset(p, patronType == "current faculty/staff")$domain),decreasing=T),5)
top_und <- head(sort(table(subset(p, patronType == "undergraduate")$domain),decreasing=T),5)
top_gra <- head(sort(table(subset(p, patronType == "graduate student")$domain),decreasing=T),5)
top_sen <- head(sort(table(subset(p, patronType == "seniors")$domain),decreasing=T),5)
top_alu <- head(sort(table(subset(p, patronType == "alumni")$domain),decreasing=T),5)

top_p <- unique(c(names(top_cfs),names(top_und),names(top_gra),names(top_sen),names(top_alu)))

exclude <- c("community and family", "retired faculty/staff", "visiting scholar")
include <- setdiff(names(table(p$patronType)),exclude)
p %>% filter(patronType %in% include) %>% filter(domain %in% top_p) %>% select(patronType, domain) %>% 
  group_by(patronType, domain) %>% summarize(counts = n()) %>% ggplot(aes(x = patronType, y = counts, fill = domain)) + geom_bar(stat='identity') + ggtitle("Patron vs most frequent domains") + xlab("PatronType") + ylab("Counts") + theme(axis.text=element_text(angle = 45,hjust = 1,size=9))

