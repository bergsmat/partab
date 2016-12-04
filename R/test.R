library(magrittr)
library(xml2)
library(dplyr)
options(project='../../puma/model_ex')
undebug(as.partab.modelname)
1360 %>% as.partab %>% as.docx %>% as.file('1360.docx')

# any and only the comment on the same line as the estimate will be scavenged.
# please keep your estimate all on one line.