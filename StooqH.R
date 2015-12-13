# Download zip files ####

zip.loc <- "./data/Stooq/"

#-Download--------------------------------------------------------------------
url_5min_hist <- "http://s.stooq.com/db/h/5_world_txt.zip"
download.file(url_5min_hist, paste0(zip.loc,"5min_hist.zip"),  mode = 'wb')

url_60min_hist <- "http://s.stooq.com/db/h/h_world_txt.zip"
download.file(url_60min_hist, paste0(zip.loc,"60min_hist.zip"),  mode = 'wb')

url_daily_hist <- "http://s.stooq.com/db/h/d_world_txt.zip"
download.file(url_daily_hist, paste0(zip.loc,"daily_hist.zip"),  mode = 'wb')
#-----------------------------------------------------------------------------