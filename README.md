library(tidyr)
library(plyr)
library(dplyr)

tbl_df(p2_mars_tianchi_user_actions)
yinyue1 <- p2_mars_tianchi_user_actions %>% group_by(V2) %>% count(V4, wt = NULL)

# unite
yinyue1 <- unite(p2_mars_tianchi_user_actions, song_time, sep = ' ', V2, V5)  
y1 <- filter(yinyue1, V4 == 1)
y2 <- filter(yinyue1, V4 == 2)
y3 <- filter(yinyue1, V4 == 3)

cy1 <- y1 %>% group_by(song_time) %>% summarise(n())
cy2 <- y2 %>% group_by(song_time) %>% summarise(n())
cy3 <- y3 %>% group_by(song_time) %>% summarise(n())
tbl_df(cy1)


hcy1 <-left_join(cy1, cy2, by = "song_time")
tbl_df(hcy1)

names(hcy1)[names(hcy1)=="n().x"]="play_num"
names(hcy1)[names(hcy1)=="n().y"]="download_num"

hcy2 <-left_join(hcy1, cy3, by = "song_time")
tbl_df(hcy2)
hcy2[is.na(hcy2)]<-0
names(hcy2)[names(hcy2)=="n()"]="collect_num"

# check user
userview <- p2_mars_tianchi_user_actions %>% group_by(V1) %>% summarise(n())

#separate
hcy3 <- separate(hcy2, song_time, c("song", "time"), sep = ' ')
tbl_df(hcy3)

names(p2_mars_tianchi_songs)[names(p2_mars_tianchi_songs)=="V2"]="artist"
names(p2_mars_tianchi_songs)[names(p2_mars_tianchi_songs)=="V1"]="song"
tbl_df(p2_mars_tianchi_songs)

hcy4 <-left_join(hcy3, p2_mars_tianchi_songs, by = "song")
tbl_df(hcy4)

library(foreign)
write.arff(hcy41, file = "hcy41.arff")
write.csv(hcy4, file = "hcy4.csv")



hcy4$time <- as.Date(hcy4$time, format="%Y%m%d")
class(hcy4$time)

hcy41$time <- as.factor(hcy41$time)

# first draft analysis in artist unit

hcy5 <-  filter(hcy4,  time =='20150702'| time =='20150703' | time =='20150704' | time == '20150705'| time == '20150706'| time == '20150707'| time == '20150708'| time == '20150709'| time == '20150710'
                | time == '20150711'| time == '20150712'| time == '20150713'| time == '20150714'| time == '20150715'| time == '20150716'| time == '20150717'| time == '20150718'| time == '20150719'| time == '20150720'
                | time == '20150721'| time == '20150722'| time == '20150723'| time == '20150724'| time == '20150725'| time == '20150726'| time == '20150727'| time == '20150728'| time == '20150729'| time == '20150730'| time == '20150731'
                | time == '20150801'| time == '20150802'| time == '20150803'| time == '20150804'| time == '20150805'| time == '20150806'| time == '20150807'| time == '20150808'| time == '20150809'| time == '20150810'
                | time == '20150811'| time == '20150812'| time == '20150813'| time == '20150814'| time == '20150815'| time == '20150816'| time == '20150817'| time == '20150818'| time == '20150819'| time == '20150820'
                | time == '20150821'| time == '20150822'| time == '20150823'| time == '20150824'| time == '20150825'| time == '20150826'| time == '20150827'| time == '20150828'| time == '20150829'| time == '20150830')

hcy6 <- unite(hcy5, artist_time, sep = ' ', artist, time)  

hcy7 <- hcy6 %>% group_by(artist_time) %>% summarise(sum = sum(play_num))

hcy8 <- separate(hcy7, artist_time, c("artist", "time"), sep = ' ')
hcy10$time <- as.Date(hcy10$time, format="%Y%m%d")
class(hcy101$time)


hcy101$time <- as.character(hcy101$time)

write.csv(hcy101, file = "hcy101.csv")
write.arff(hcy81, file = "hcy81.arff")


hcy9 <- hcy8 %>% group_by(artist) %>% summarise(avg = mean(sum))
write.csv(hcy9, file = "hcy9.csv")
hcy10 <- left_join(hcy8, hcy9, by = "artist")
write.csv(hcy10, file = "hcy10.csv")


