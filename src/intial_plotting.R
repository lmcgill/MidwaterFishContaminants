##### This R Script will be used to visualize pilot data 

# install and/or load the necessary R packages
if("pacman" %in% installed.packages() == FALSE){install.packages("pacman")}
pacman::p_load(geojsonR,sf,dplyr, ggplot2, maps, lubridate, tidyr,ggh4x,cowplot,
               marmap,sjPlot,rgeos, tidyverse, plyr) 

##################################################################################################################
################### Read in inital data   ###################
##################################################################################################################

samples = read.csv(here::here("data","CalCOFI_data","myctophid_samples_19Oct2023.csv"), skip=1) %>% 
  dplyr::select(UID, SpeciesName, Line.Station, Date, Pool, Region, Standard.Length..mm., 
                Gape.Width...Upper..mm.,Gape.Width...Lower..mm., Gape.Width...Width..mm., 
                Weight.before.tissue.removal..g., Weight.after.tissue...stomach.removal..g., 
                SIA...CSIAA.Sample.Name) %>% 
  dplyr::mutate(Date = as.Date(Date, format="%m/%d/%y"))

results.ddt = read.csv(here::here("data","contaminant_data","raw","myctophid_samples_19Oct2023.csv")) %>% 
  dplyr::select(Pool, Year, X24DDE:ww.g)

results.15N = read.csv(here::here("data","isotope_data","bulk_15N_data.csv")) %>% 
  dplyr::select(Sample.ID, d15NAir, TotalN_µg)

##################################################################################################################
################### Plot  ###################
##################################################################################################################

data = samples %>% 
  left_join(., results.ddt, by="Pool") %>% 
  left_join(., results.15N, by=c("SIA...CSIAA.Sample.Name" = "Sample.ID")) %>% 
  #dplyr::filter(is.na(δ15NAir....) == FALSE) %>% 
  dplyr::mutate(Year = year(Date))

data.summarized = samples %>% 
  dplyr::filter(Pool %in% results.ddt$Pool) %>% 
  left_join(., results.15N, by=c("SIA...CSIAA.Sample.Name" = "Sample.ID")) %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::summarize(Length.mm = mean(Standard.Length..mm.), 
                   Weight.g = mean(Weight.after.tissue...stomach.removal..g.), 
                   Region = unique(Region), 
                   d15NAir = mean(d15NAir, na.rm=TRUE), 
                   TotalN_µg = mean(TotalN_µg, na.rm=TRUE)) %>% 
  dplyr::ungroup() %>% 
  left_join(., results.ddt, by="Pool") %>% 
  tidyr::gather(X24DDE:X44DDMU, key="DDX", value="Value.ng") %>% 
  dplyr::mutate(DDX = factor(DDX, levels=c("X24DDE", "X44DDE", "X24DDD", "X44DDD", 
                                           "X24DDT", "X44DDT", "X44DDMU"))) 

data.summarized %>% 
  #dplyr::filter(!Pool %in% c("7","8","9")) %>% 
  dplyr::filter(Pool %in% c("9", "7", "8")) %>% 
  ggplot(aes(x=DDX, y=Value.ng/Lipid.g)) + 
  geom_bar(stat="identity")+
  facet_grid(~Pool) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

data %>% 
  dplyr::filter(Region != "") %>% 
  ggplot(aes(x= Standard.Length..mm., y=d15NAir)) + 
  geom_point(mapping=aes(color=Region)) + 
  theme_bw() 
  
data.summarized %>% 
  #dplyr::filter(!Pool %in% c("7","8","9")) %>% 
  dplyr::filter(!Pool %in% c("9", "7")) %>% 
  ggplot(aes(x=DDX, y=Value.ng/Lipid.g)) + 
  geom_bar(stat="identity")+
  facet_grid(Region~Year) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng)) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year, y=totalDDX/Lipid.g)) + 
  geom_point(aes(size=Length.mm, color=d15NAir)) + 
  geom_line()+ 
  facet_grid(~Region) + 
  theme_bw() + 
  scale_color_gradient2(midpoint=14.8, mid="yellow")
  
data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng), 
                totalDDE = sum(Value.ng[DDX %in% c("X24DDE","X44DDE")])) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year, y=totalDDE/totalDDX)) + 
  geom_point(aes(size=Weight.g)) + 
  geom_line()+ 
  facet_grid(~Region) 


data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng), 
                totalDDE = sum(Value.ng[DDX %in% c("X24DDD","X44DDD")])) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year, y=totalDDE/totalDDX)) + 
  geom_point(aes(size=Weight.g)) + 
  geom_line()+ 
  facet_grid(~Region) 


data.summarized %>% 
  dplyr::group_by(Pool) %>% 
  dplyr::mutate(totalDDX = sum(Value.ng)) %>% 
  dplyr::ungroup() %>% 
  ggplot(aes(x=Year , y=d15NAir  )) + 
  geom_point(aes(size=totalDDX/Lipid.g, color=totalDDX/Lipid.g)) + 
  #geom_line()+ 
  facet_grid(~Region) + 
  theme_bw() 