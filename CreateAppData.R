
Batting2 <- Lahman::Batting %>%
  left_join(select(Lahman::Master, nameLast, nameFirst, debut, finalGame, playerID, birthYear), by = 'playerID') %>%
  filter(is.na(birthYear) == FALSE &
           yearID >= 1950) %>%
  mutate(debutyear = ifelse(is.na(as.numeric(str_sub(debut,0,4))), 
                            as.numeric(str_sub(debut, -4)),
                            as.numeric(str_sub(debut, 0, 4))),
         retireyear = ifelse(is.na(as.numeric(str_sub(finalGame,0,4))),
                             as.numeric(str_sub(finalGame, -4)),
                             as.numeric(str_sub(finalGame, 0, 4))),
         Debut_Age = debutyear - birthYear,
         Max_Age = retireyear - birthYear,
         name = str_c(nameLast, nameFirst, (str_c(debutyear, retireyear, sep = ' - ')), sep = ', '))

write.csv(Batting2, 'Batting2.csv', row.names = F)