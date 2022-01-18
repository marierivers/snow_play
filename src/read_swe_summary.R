#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
read_swe_summary <- function(files) {
  path <- here("data", "WY2021", "SWESummary", files)
  swe_daily <- read.csv(path, header = TRUE, sep = ",", na = "NaN") %>% 
    drop_na(Lat) %>% 
    rename(site_name = Name) %>% 
    rename(state = State) %>% 
    rename(lat = Lat) %>% 
    rename(lon = Lon) %>% 
    rename(elevation_m = Elev_m) %>% 
    rename(pct_of_median_longterm_swe = normSWE) %>% 
    rename(daily_swe_change_inches = dSWE) %>% 
    mutate(file_name = files) %>% 
    mutate_at("state", str_replace, "US", "") %>% 
    mutate(water_year = "wy2021") %>% 
    mutate(date = str_replace(file_name,
                              pattern = "SnowToday_USwest_",
                              replacement = "")) %>% 
    mutate(date = str_replace(date,
                              pattern = "_SWEsummary.txt",
                              replacement = "")) %>% 
    mutate(date = lubridate::ymd(date)) %>% 
    mutate(year = lubridate::year(date)) %>% 
    mutate(month = lubridate::month(date)) %>% 
    mutate(day = lubridate::day(date))
}