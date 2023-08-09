library(tigris)
library(dplyr)
library(ggplot2)
library(data.table)
library(readr)
library(readxl)
library(ggplot2)
library(tidycensus)
library(fractional)
library(glue)

rm(list=ls());cat('\f');gc()

county_pop <- tidycensus::get_estimates("County", variables = "POP", state = "NC", 
                                        show_call = T)
county_pop$NAME <- county_pop$NAME %>%
  gsub(" County.*$", "", .)

county_pop <- county_pop[,c(1,4)]
colnames(county_pop) <- c("NAME", "population_2019")

nc.counties <- tigris::counties(state = "NC", year = 2020)


nc.counties <- left_join(nc.counties, county_pop)


pit22 <- read_tsv("County	Total House-holds	Total People	Children 17 & Under	Adults 18-24	Adults Age 25+	Total House-holds	Total People	Adults 18-24	Adults Age 25+	Total House-holds	Children 17 & Under	TOTAL PEOPLE EXPERIENCING HOMELESSNESS
Alamance	4	9	5	1	3	19	19	2	17	0	0	28
Alexander	0	0	0	0	0	6	6	1	5	0	0	6
Anson	4	10	6	0	4	2	2	0	2	0	0	12
Beaufort	3	10	7	0	3	12	12	0	12	0	0	22
Bertie	0	0	0	0	0	0	0	0	0	0	0	0
Bladen	0	0	0	0	0	0	0	0	0	0	0	0
Burke	3	10	7	0	3	47	48	2	46	0	0	58
Cabarrus	17	44	27	0	17	50	52	0	52	0	0	96
Caldwell	7	19	12	0	7	23	23	0	23	0	0	42
Camden	0	0	0	0	0	0	0	0	0	0	0	0
Carteret	1	2	1	0	1	5	5	0	5	0	0	7
Caswell	0	0	0	0	0	0	0	0	0	0	0	0
Catawba	35	108	60	2	46	138	147	16	131	6	6	261
Chatham	2	9	7	0	2	16	20	4	16	0	0	29
Cherokee	3	10	6	1	3	13	13	0	13	0	0	23
Chowan	0	0	0	0	0	0	0	0	0	0	0	0
Clay	0	0	0	0	0	1	1	0	1	0	0	1
Columbus	0	0	0	0	0	0	0	0	0	0	0	0
Craven	1	4	3	0	1	22	22	3	19	0	0	26
Currituck	0	0	0	0	0	0	0	0	0	0	0	0
Dare	0	0	0	0	0	16	17	0	17	0	0	17
Davidson	6	15	8	1	6	60	61	7	54	0	0	76
Davie	2	8	6	0	2	0	0	0	0	0	0	8
Duplin	0	0	0	0	0	0	0	0	0	0	0	0
Edgecombe	10	33	22	1	10	6	6	0	6	0	0	39
Franklin	1	2	1	0	1	0	0	0	0	0	0	2
Gates	0	0	0	0	0	4	4	0	4	0	0	4
Graham	0	0	0	0	0	1	1	0	1	0	0	1
Granville	0	0	0	0	0	3	3	0	3	0	0	3
Greene	0	0	0	0	0	0	0	0	0	0	0	0
Halifax	1	2	1	0	1	6	6	0	6	0	0	8
Harnett	1	5	3	1	1	5	5	0	5	0	0	10
Haywood	6	19	12	1	6	188	189	4	185	0	0	208
Henderson	10	29	15	5	9	110	111	3	108	0	0	140
Hertford	1	3	2	0	1	1	1	0	1	0	0	4
Hoke	3	9	6	0	3	2	2	0	2	0	0	11
Hyde	0	0	0	0	0	0	0	0	0	0	0	0
Iredell	7	29	21	0	8	86	86	4	82	0	0	115
Jackson	5	13	6	1	6	22	27	2	25	0	0	40
Johnston	8	26	16	1	9	22	23	1	22	0	0	49
Jones	0	0	0	0	0	0	0	0	0	0	0	0
Lee	8	24	16	0	8	72	72	3	69	0	0	96
Lenoir	4	22	12	2	8	22	22	3	19	0	0	44
Macon	1	3	2	0	1	3	3	0	3	0	0	6
Madison	4	14	10	1	3	2	2	0	2	0	0	16
Martin	0	0	0	0	0	0	0	0	0	0	0	0
McDowell	2	5	3	1	1	45	45	3	42	0	0	50
Montgomery	0	0	0	0	0	21	21	0	21	0	0	21
Moore	6	16	10	1	5	8	9	3	6	0	0	25
Nash	2	10	7	0	3	59	60	0	60	0	0	70
Northampton	0	0	0	0	0	0	0	0	0	0	0	0
Onslow	4	17	12	0	5	47	49	6	43	0	0	66
Pamlico	0	0	0	0	0	0	0	0	0	0	0	0
Pasquotank	4	13	8	0	5	13	15	1	14	0	0	28
Perquimans	0	0	0	0	0	0	0	0	0	0	0	0
Person	2	6	4	0	2	3	3	0	3	0	0	9
Pitt	4	12	8	0	4	56	57	3	54	0	0	69
Polk	1	4	3	0	1	4	4	0	4	0	0	8
Randolph	2	6	4	0	2	2	2	1	1	0	0	8
Richmond	11	41	28	5	8	50	53	5	48	0	0	94
Robeson	6	22	13	0	9	44	48	5	43	0	0	70
Rockingham	0	0	0	0	0	8	8	1	7	0	0	8
Rowan	3	8	5	1	2	82	82	1	81	0	0	90
Rutherford	6	16	9	1	6	21	21	3	18	0	0	37
Sampson	0	0	0	0	0	0	0	0	0	0	0	0
Scotland	0	0	0	0	0	6	7	0	7	0	0	7
Stanly	4	12	8	0	4	12	12	1	11	0	0	24
Stokes	0	0	0	0	0	1	1	0	1	0	0	1
Surry	7	23	13	2	8	37	38	0	38	0	0	61
Swain	0	0	0	0	0	2	3	0	3	0	0	3
Transylvania	6	23	15	1	7	13	13	0	13	0	0	36
Tyrrell	0	0	0	0	0	0	0	0	0	0	0	0
Union	9	35	23	1	11	92	101	11	90	0	0	136
Vance	3	7	4	0	3	17	17	0	17	0	0	24
Warren	0	0	0	0	0	0	0	0	0	0	0	0
Washington	0	0	0	0	0	0	0	0	0	0	0	0
Wayne	3	10	5	1	4	4	4	1	3	0	0	14
Wilson	3	9	5	0	4	14	15	0	15	0	0	24
Yadkin	0	0	0	0	0	1	1	0	1	0	0	1
") %>%
  .[,c(1,13)]

county_regions <- read_tsv(I("County	Region
Cherokee	Region 01
Clay	Region 01
Graham	Region 01
Haywood	Region 01
Jackson	Region 01
Macon	Region 01
Madison	Region 01
Swain	Region 01
Duplin	Region 10
Greene	Region 10
Lenoir	Region 10
Sampson	Region 10
Wayne	Region 10
Wilson	Region 10
Camden	Region 11
Chowan	Region 11
Currituck	Region 11
Dare	Region 11
Gates	Region 11
Hertford	Region 11
Pasquotank	Region 11
Perquimans	Region 11
Tyrrell	Region 11
Beaufort	Region 12
Bertie	Region 12
Hyde	Region 12
Martin	Region 12
Pitt	Region 12
Washington	Region 12
Carteret	Region 13
Craven	Region 13
Jones	Region 13
Onslow	Region 13
Pamlico	Region 13
Henderson	Region 02
Polk	Region 02
Rutherford	Region 02
Transylvania	Region 02
Alexander	Region 03
Burke	Region 03
Caldwell	Region 03
Catawba	Region 03
McDowell	Region 03
Davie	Region 04
Iredell	Region 04
Stokes	Region 04
Surry	Region 04
Yadkin	Region 04
Cabarrus	Region 05
Davidson	Region 05
Rowan	Region 05
Stanly	Region 05
Union	Region 05
Alamance	Region 06
Caswell	Region 06
Chatham	Region 06
Person	Region 06
Rockingham	Region 06
Anson	Region 07
Harnett	Region 07
Hoke	Region 07
Johnston	Region 07
Lee	Region 07
Montgomery	Region 07
Moore	Region 07
Randolph	Region 07
Richmond	Region 07
Bladen	Region 08
Columbus	Region 08
Robeson	Region 08
Scotland	Region 08
Edgecombe	Region 09
Franklin	Region 09
Granville	Region 09
Halifax	Region 09
Nash	Region 09
Northampton	Region 09
Vance	Region 09
Warren	Region 09
"))

pit22 <- full_join(pit22, county_regions)


colnames(pit22) <- c("NAME", "Total_People_Experiencing_Homelessness", "Region")

pit22

nc.counties <- left_join(nc.counties, pit22)

nc.regions <- nc.counties %>%
  group_by(Region) %>%
  summarise(t_pop = sum(population_2019, na.rm = T), 
            t_hl = sum(Total_People_Experiencing_Homelessness, na.rm = T), 
            rat_hl2 = t_hl/t_pop, 
            rat_hl = t_pop/t_hl)



nc.counties$rate_of_homelessness2 <- nc.counties$Total_People_Experiencing_Homelessness / 
  nc.counties$population_2019

nc.counties$rate_of_homelessness <- nc.counties$population_2019 / 
  nc.counties$Total_People_Experiencing_Homelessness 

rc.br <- seq(1000,5000,by = 1000)

nc.places <- tigris::places("NC", cb = T, year = 2020)


nc.native <- tigris::native_areas(cb = T)
nc.native$geometry

findnativelonlat <- NULL
n_id <- 0 
for(i in 1:length(nc.native$geometry)){
  temp.name <- nc.native$NAME[i]
  for(i2 in 1:length(nc.native$geometry[[i]])){
    for(i3 in 1:length(nc.native$geometry[[i]][[i2]]))
      n_id <- n_id + 1
    
    findnativelonlat <- rbind(findnativelonlat, 
                              data.frame(name = temp.name, 
                                         n_id = n_id, 
                                         i_1 = i, 
                                         i_2 = i2, 
                                         i_3 = i3, 
                                         lon = nc.native$geometry[[i]][[i2]][[i3]][,1], 
                                         lat = nc.native$geometry[[i]][[i2]][[i3]][,2]))
     
  }
}

findnativelonlat[findnativelonlat$lon > -85 & 
                   findnativelonlat$lon < -75 & 
                   findnativelonlat$lat < 37 & 
                   findnativelonlat$lat > 33.5,]

ggplot()+
  geom_sf(data = nc.counties) + 
  #geom_sf(data = nc.native)+
  geom_polygon(data = findnativelonlat[findnativelonlat$lon > -85 & 
                                      findnativelonlat$lon < -75 & 
                                      findnativelonlat$lat < 37 & 
                                      findnativelonlat$lat > 33.5 & 
                                        !findnativelonlat$name %in% 
                                        c("Upper South Carolina Pee Dee", 
                                          "Pee Dee", 
                                          "Waccamaw", 
                                          "Catawba",
                                          "Beaver Creek",
                                          "Georgia Tribe of Eastern Cherokee"),], 
            aes(x = lon, y = lat, group = n_id, 
                fill = name), color = NA) 

View(nc.native$geometry)

x1 <- tigris::tribal_subdivisions_national()
tigris::tribal_census_tracts()
?tigris::tribal_block_groups()


ggplot() + 
  geom_sf(data = nc.native[nc.native$LSAD %in% c("43","25"),], 
          aes(fill = GEOID))+
  theme(legend.position = "none")

ggplot(data = mutate(nc.regions, 
                     rat_hl = ifelse(is.na(Region), NA, rat_hl)), 
       aes(fill = rat_hl), 
       color = "black") + 
  geom_sf()+
  #geom_sf(data = nc.places, color = NA, fill = "black", alpha = 0.5)+
  # scale_fill_gradient(na.value = "#EFF0F2", 
  #                     high = "navy blue", low = "cyan", #mid = "white", 
  #                     # midpoint = median(mutate(nc.regions, 
  #                     #                          rat_hl = ifelse(is.na(Region), NA, rat_hl))$rat_hl, na.rm = T)
  #                     name = NULL, 
  #                     labels = glue("1 in {scales::comma(rc.br, accuracy = 1)}", sep = ","))+
scale_fill_gradient2(na.value = "#EFF0F2",
                    low = "dark red", high = "dark green", mid = "yellow",
                     midpoint = median(mutate(nc.regions,
                                              rat_hl = ifelse(is.na(Region), NA, rat_hl))$rat_hl, na.rm = T),
                    name = NULL,
                    labels = glue("1 in {scales::comma(rc.br, accuracy = 1)}", sep = ","))+
  # scale_fill_viridis_c(option = "H",
  #                      direction = -1,
  #                      labels = glue("1 in {scales::comma(rc.br, accuracy = 1)}", sep = ","),
  #                      name = "Rate of Homelessness\n(2022)",
  #                      breaks = rc.br,
  #                      na.value = "#EFF0F2")+
  theme_void()+
  theme(legend.position = "bottom", 
        legend.direction = "vertical", 
        legend.text = element_text(angle = 0, hjust = 0, vjust = 0.5),
        plot.title = element_text(hjust = 0.5,vjust = 0.5), 
        plot.caption = element_text(hjust = 0.5,vjust = 0.5),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_), # necessary to avoid drawing panel outline
        #panel.grid.major = element_blank(), # get rid of major grid
        #panel.grid.minor = element_blank(), # get rid of minor grid
        plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_), # necessary to avoid drawing plot outline
        legend.background = element_rect(fill = "transparent", 
                                         colour = NA_character_),
        legend.box.background = element_rect(fill = "transparent", 
                                             colour = NA_character_),
        legend.key = element_rect(fill = "transparent", 
                                  colour = NA_character_))+
  labs(title = "Rate of Homelessness for NCCEH CoC Regions, 2022", 
       caption = "Source: 2022 PIT Data, 2019 US Census Bureau Population Estimates API")
