.libPaths('lib')
library(xml2)
library(dplyr)
library(tidyr)
library(magrittr)
source('metaMerge.R')
source('shuffle.R')
source('contains.R')
source('as.best.R')
source('xpath.R')
source('halfmatrix.R')
source('bootstrap.R')
source('partab.R')
options(project='model')

1360 %>% xpath('//etashrink/row/col')
1360 %>% xpath('//theta/val')
1360 %>% xpath('//theta/val/@name')
1360 %>% xpath('//thetase/val')
1360 %>% xpath('//thetafoo/val')
1360 %>% xpath('//omega/row/col')
1360 %>% xpath('//omegase/row/col')
1360 %>% xpath('//sigma/row/col')
1360 %>% xpath('//sigma/row/col') %>% as.halfmatrix %>% as.data.frame
1360 %>% as.bootstrap

1360 %>% as.partab(F)










