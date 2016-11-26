.libPaths('lib')
library(xml2)
library(magrittr)

x <- read_xml('CONTROL5.xml')
x %>% xml_find_first('nm:start_datetime') %>% xml_text
x %>% xml_find_first('start_datetime')
