#' Read the NOAA data set in a dataframe manner.
#'
#' This function reads the raw NOAA earthquake data from the base file and
#' converts it into a clean dataframe for further analysis.
#'
#' @param Dataset in a tab-delimited format.
#'
#' @return This function convert the raw input into a dataframe and returns
#' a tibble as an output.
#'
#' @importFrom readr read_delim
#' @importFrom dplyr tbl_df
#'
#' @note If the base file does NOT exist at the specified directory, the function
#' throws an error message and stops executing.
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' eq_data_read(filename)
#' }
#'
#' @export
eq_data_read <- function(filename) {

        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_delim(filename, delim='\t',progress = FALSE)
        })
        dplyr::tbl_df(data)

}






#' Clean the location name column in the dataframe.
#'
#' This function cleans the location name column and strip out the country name
#' in the dataframe.
#'
#' @param Input cleaned data.frame from the base file dataset.
#'
#' @return This function converts the location name column of the dataframe into
#' the country name and returns a tibble.
#'
#' @importFrom dplyr filter_ mutate_ "%>%"
#' @importFrom stringr str_detect str_match str_to_title
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_location_clean()
#' }
#'
#' @export
eq_location_clean <- function(eq_cleanup) {

        #test that LOCATION_NAME column is present
        all_columns <- colnames(eq_cleanup)
        stopifnot(any('LOCATION_NAME' %in% all_columns))

        #handle outlier cases so they work with regex
        #one LOCATION_NAME is NA, replace with COUNTRY
        eq_cleanloc1 <- eq_cleanup %>% dplyr::mutate_(LOCATION_NAME = ~ ifelse(is.na(LOCATION_NAME),COUNTRY,LOCATION_NAME))

        #if LOCATION_NAME=COUNTRY, add COUNTRY to LOCATION_NAME so LOCATION_NAME is retained
        eq_cleanloc2 <- eq_cleanloc1 %>%
                dplyr::mutate_(LOCATION_NAME = ~ ifelse(LOCATION_NAME==COUNTRY, paste0(COUNTRY,":  ",LOCATION_NAME),LOCATION_NAME))

        #if there is no COUNTRY in LOCATION_NAME field, add KEEP: so that regex won't strip LOCATION_NAME
        eq_cleanloc3 <- eq_cleanloc2 %>%
                dplyr::mutate_(LOCATION_NAME = ~ ifelse(!stringr::str_detect(LOCATION_NAME,":"), paste0("KEEP:  ",LOCATION_NAME),LOCATION_NAME))

        #regex to clean up LOCATION_NAME - removes first COUNTRY name only
        #regex includes punctuation as some COUNTRY names include punctuation
        eq_cleanloc4 <- eq_cleanloc3 %>%
                dplyr::mutate_(LOCATION_NAME = ~ stringr::str_match(LOCATION_NAME,"^[a-zA-Z\\s()-;',\\.]+:\\s*(.*)")[,2])

        #check for any blank values
        #eq_cleanloc5 <- eq_cleanloc4 %>% dplyr::mutate_(LOCATION_NAME = ~ ifelse(LOCATION_NAME=="",COUNTRY,LOCATION_NAME))

        #convert LOCATION_NAME to title format
        eq_cleanloc5 <- eq_cleanloc4 %>% dplyr::mutate_(LOCATION_NAME = ~ stringr::str_to_title(LOCATION_NAME))


}







#' Clean the dataframe then create and add a DATE column.
#'
#' This function generates a DATE column and add in to the cleaned dataframe
#' and it changes the class of the variables LATITUDE, LONGITUDE, EQ_PRIMARY and
#' DEATHS to numeric.
#'
#' @source This function calls the function eq_location_clean() within it.
#'
#' @param This function takes the cleaned dataframe as inputs.
#'
#' @return This function creates a cleaned dataframe and returns a tibble.
#'
#' @importFrom dplyr filter_ mutate_ "%>%"
#' @importFrom tidyr replace_na unite_
#' @importFrom lubridate as_date
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data()
#' }
#'
#' @export
eq_clean_data <- function(raw_df){
        all_columns <- colnames(raw_df)
        stopifnot(any('YEAR' %in% all_columns),any('MONTH' %in% all_columns),
                  any('DAY' %in% all_columns),any('LATITUDE' %in% all_columns),
                  any('LONGITUDE' %in% all_columns),any('EQ_PRIMARY' %in% all_columns),
                  any('DEATHS' %in% all_columns),any('LOCATION_NAME' %in% all_columns))

                tryCatch({
                eq_clean1<-raw_df %>% tidyr::replace_na(list(MONTH = 1,DAY = 1))%>%
                        dplyr::filter_(~ YEAR > 0) %>%
                        tidyr::unite_('DATE',c('YEAR','MONTH','DAY'),sep = '-') %>%
                        dplyr::mutate_(DATE = ~ lubridate::as_date(DATE))
        }, error = function(e) {
                stop("YEAR,MONTH,DAY not converted to DATE")
                return(NULL)
        })


        eq_clean2<-eq_clean1 %>% dplyr::mutate_(LATITUDE = ~ as.numeric(LATITUDE),LONGITUDE = ~ as.numeric(LONGITUDE),
                                                EQ_PRIMARY = ~ as.numeric(EQ_PRIMARY),DEATHS = ~ as.numeric(DEATHS))


        eq_clean<-eq_location_clean(eq_clean2)

        return(eq_clean)

}







#' Visualize the information in the NOAA earthquakes dataset.
#'
#' This function is a tool to visualize the information in the NOAA base file
#' dataset. This function plots a time line of earthquakes ranging from xmin
#' to xmaxdates with a point for each earthquake.
#'
#' @param filtered dataframe for any country and date.
#'
#' @return This function returns layer of plot.
#'
#' @importFrom ggplot2 layer ggproto
#' @importFrom grid pointsGrob gpar
#' @import ggmap
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000) %>%
#' ggplot() +
#' geom_timeline(aes(x=DATE,size=EQ_PRIMARY,color=DEATHS),alpha=0.5)
#'
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter((COUNTRY=="USA" | COUNTRY=="CHINA") & lubridate::year(DATE) >= 2000) %>%
#' ggplot() +
#' geom_timeline(aes(x=DATE,y=COUNTRY,size=EQ_PRIMARY,color=DEATHS),alpha=0.5)
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {


        ggplot2::layer(
                geom = GeomTimeLine, mapping = mapping,
                data = data, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )


}
# Geom definition for GeomTimeLine
GeomTimeLine <- ggplot2::ggproto("GeomTimeLine", ggplot2::Geom,
                        required_aes = c("x"),
                        non_missing_aes = c("size", "shape", "colour"),
                        default_aes = ggplot2::aes(y = 0.05,
                                          shape = 19, colour = "gray", alpha=0.4,
                                          size = 4, stroke = 0.5, fill=NA
                        ),
                        draw_key = ggplot2::draw_key_point,
                        draw_group = function(data, panel_scales, coord) {

                                ## Transform the data
                                coords <- coord$transform(data,panel_scales)

                                ## Construct a grid grob
                                grid::pointsGrob(
                                        coords$x,
                                        coords$y,
                                        pch=coords$shape,
                                        gp = grid::gpar(
                                                size = coords$size,
                                                col = alpha(coords$colour, coords$alpha),
                                                fill = alpha(coords$fill, coords$alpha),
                                                # Stroke is added around the outside of the point
                                                fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                lwd = coords$stroke * .stroke / 2
                                        )

                                )
                        }
)







#' Add annotations to the earthquake data visualization.
#'
#' This function (geom) adds a vertical line to each data point with a text
#' annotation (e.g. the location of the earthquake) attached to each line.
#'
#' @param filtered dataframe for any country and dates.
#'
#' @return This function returns a layer of plot.
#'
#' @importFrom dplyr filter_ group_by_ min_rank "%>%"
#' @importFrom ggplot2 layer ggproto
#' @importFrom grid segmentsGrob textGrob gpar gTree
#' @import ggmap
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' sample_USA <- readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000)
#'
#' ggplot(data = sample_USA, aes(x=DATE)) + geom_timeline() +
#' geom_timeline_label(data=sample_USA,aes(label=LOCATION_NAME),n_max=8) + theme_classic()
#'
#' ggplot() +
#' geom_timeline_label(data=sample_USA,aes(x=DATE,label=LOCATION_NAME))
#'
#' sample_2_countries<- readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter((COUNTRY=="USA" | COUNTRY=="CHINA") & lubridate::year(DATE) >= 2000)
#'
#' ggplot() +
#' geom_timeline_label(data=sample_2_countries,aes(x=DATE,y=COUNTRY,label=LOCATION_NAME))
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, n_max = 5, ...) {


        #top n_max using NSE
        data2 <- data %>% dplyr::group_by_(~ COUNTRY) %>%
                dplyr::filter_(~ min_rank(desc(EQ_PRIMARY)) <= n_max)

        #equivalent to top_n call:
        #data2 <- data %>% dplyr::group_by_(~ COUNTRY) %>% dplyr::top_n(n_max,EQ_PRIMARY)

        ggplot2::layer(
                geom = GeomTimeLineLabel, mapping = mapping,
                data = data2, stat = stat, position = position,
                show.legend = show.legend, inherit.aes = inherit.aes,
                params = list(na.rm = na.rm, ...)
        )

}
# Geom definition for GeomTimeLineLabel
GeomTimeLineLabel <- ggplot2::ggproto("GeomTimeLineLabel", ggplot2::Geom,
                             required_aes = c("x","label"),
                             default_aes = ggplot2::aes(y=0.1, colour = "black", size = 0.2,
                                               linetype = 1, alpha = NA, angle = 45,
                                               hjust = 0, vjust = 0,
                                               family = "", fontface = 2, lineheight = 1.5),
                             draw_key = ggplot2::draw_key_label,
                             draw_group = function(data,panel_scales,coord) {


                                     ## Transform the data
                                     coords <- coord$transform(data,panel_scales)

                                     #create grid grob
                                     # 2 parts - vertical line and text label

                                     vline <- grid::segmentsGrob(
                                             x0 = coords$x, x1 = coords$x,
                                             y0 = coords$y, y1 = coords$y + 0.1,
                                             default.units = "native",
                                             gp = grid::gpar(
                                                     size=0.5,
                                                     alpha=1,
                                                     color="black")
                                     )

                                     line_annot <- grid::textGrob(
                                             coords$label,
                                             x=coords$x,
                                             y=coords$y + 0.1,
                                             default.units = "native",
                                             hjust = coords$hjust, vjust = coords$vjust,
                                             rot = coords$angle,
                                             gp = grid::gpar(
                                                     col = "black",
                                                     alpha=1,
                                                     fontsize = 3.5 * .pt,
                                                     fontfamily = coords$family,
                                                     fontface = coords$fontface,
                                                     lineheight = coords$lineheight
                                             )
                                     )
                                     timeline_label <- grid::gTree(children = grid::gList(vline,line_annot))
                             }

)







#' Create interactive map visualization of NOAA earthquake data.
#'
#' This function maps the epicenters (LATITUDE/LONGITUDE) and annotates each
#' point with in pop up window containing annotation data stored in a column of
#' the cleaned data frame of NOAA dataset.
#'
#' @param The clean dataframe of the base NOAA dataset.
#' @param annot_col for "DATE".
#'
#' @return This function returns an interactive map.
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
#' }
#' @export
eq_map <- function(eq_clean=NULL, annot_col="DATE"){

        #test that correct columns are present
        all_columns <- colnames(eq_clean)

        stopifnot(any('DATE' %in% all_columns),any('LATITUDE' %in% all_columns),
                  any('LONGITUDE' %in% all_columns),any('EQ_PRIMARY' %in% all_columns))

        #check to see if invalid column provided - print message and default to DATE
        if(!(any(annot_col %in% all_columns))) {
                warning("Invalid Column - DATE Displayed")
                annot_col = "DATE"
        }

        #call to leaflet
        leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::addCircleMarkers(data = eq_clean, lng = ~ LONGITUDE, lat = ~ LATITUDE, radius = ~ EQ_PRIMARY,
                                          weight=1, fillOpacity = 0.2, popup =~ paste(get(annot_col)))

}







#' Create popup text for markers for a earthquake point.
#'
#' This function puts together a character string for each earthquake that will
#' show the cleaned location on the map.
#'
#' @param cleaned and filtered dataframe for any location and dates.
#'
#' @return This function returns a map in HTML form with some text marker.
#'
#' @importFrom dplyr mutate_ select_ collect "%>%"
#' @importFrom tidyr unite_
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(eq_clean=NULL) {

        #test that correct columns are present
        all_columns <- colnames(eq_clean)

        stopifnot(any('LOCATION_NAME' %in% all_columns),any('EQ_PRIMARY' %in% all_columns),
                  any('DEATHS' %in% all_columns))

        #use dplyr to create values for "popup_text" column
        #remove labels for values with NA
        #check for empty string - return "All Values are NA"
        data2<- eq_clean %>% dplyr::select_(.dots=c('LOCATION_NAME','EQ_PRIMARY','DEATHS')) %>%
                dplyr::mutate_(new_LOCATION_NAME = ~ ifelse(is.na(LOCATION_NAME), LOCATION_NAME, paste0("<b>Location:</b> ", LOCATION_NAME,"<br />"))) %>%
                dplyr::mutate_(new_EQ_PRIMARY = ~ ifelse(is.na(EQ_PRIMARY), EQ_PRIMARY, paste0("<b>Magnitude:</b> ", EQ_PRIMARY,"<br />"))) %>%
                dplyr::mutate_(new_DEATHS = ~ ifelse(is.na(DEATHS), DEATHS, paste0("<b>Total Deaths:</b> ", DEATHS))) %>%
                tidyr::unite_('popup_values',c('new_LOCATION_NAME','new_EQ_PRIMARY','new_DEATHS'),sep ='') %>%
                dplyr::mutate_(popup_values = ~ stringr::str_replace_all(popup_values,"[,]*NA[,]*","")) %>%
                dplyr::mutate_(popup_values = ~ ifelse(popup_values=="","All Values are NA",popup_values))

        popup_values <- dplyr::collect(dplyr::select_(data2,.dots=c('popup_values')))[[1]]

        return(popup_values)

}







#' Create a timeline visualization for NOAA earthquake base data.
#'
#' This function generates a timeline visualization for the NOAA
#' earthquake base data.  The data should be subsetted in order to create
#' an understandable visualization.  There are several ways to call
#' the function so that different visualizations are generated.  The
#' function is a wrapper function of all previously defined functions.
#'
#' @source This function calls geom_timeline() and geom_timeline_label functions.
#'
#' @param cleaned and filtered data.frame.
#' @param size color alpha max y=COUNTRY
#'
#' @return This function returns a timeline visualization for a subset
#' of the earthquake data.
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' filename<-system.file("extdata","data.gz",package="NOAAA")
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000) %>%
#' eq_time(size="EQ_PRIMARY",color="DEATHS")
#'
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "USA" & lubridate::year(DATE) >= 2000) %>%
#' eq_time(size="EQ_PRIMARY",color="DEATHS",alpha=0.5,timeline_label=TRUE,n_max=8)
#'
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter((COUNTRY=="USA" | COUNTRY=="CHINA") & lubridate::year(DATE) >= 2000) %>%
#' eq_time(y="COUNTRY",color="DEATHS",alpha=0.5)
#'
#' readr::read_delim(filename, delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter((COUNTRY=="USA" | COUNTRY=="CHINA") & lubridate::year(DATE) >= 2000) %>%
#' eq_time(y="COUNTRY",color="DEATHS",alpha=0.5,timeline_label=TRUE)
#' }
#'
#' @export
eq_time <- function(eq_clean=NULL,y=NULL,size=NULL,color=NULL,alpha=0.4,timeline_label=FALSE,n_max=5){

        #check to see if invalid input values provided

        #test that DATE column is present
        all_columns <- colnames(eq_clean)
        stopifnot(any('DATE' %in% all_columns))

        if(! is.null(y)) {
                if(y !="COUNTRY") {
                        stop("Invalid column for y - use COUNTRY")
                }
        }

        if(!is.null(size)) {
                if(size != "EQ_PRIMARY") {
                        stop("Invalid column for size - use EQ_PRIMARY")
                }
        }

        if(!is.null(color)) {
                if(color != "DEATHS") {
                        stop("Invalid column for color - use DEATHS")
                }
        }

        if(!is.logical(timeline_label)) {
                warning("Invalid column for timeline_label - set to FALSE")
                timeline_label=FALSE
        }

        if(!is.null(alpha)) {
                if(is.numeric(alpha)){
                        if(alpha <0  | alpha > 1) {
                                warning("Invalid value for alpha - set alpha to 0.4")
                                alpha = 0.4
                        }
                }else{
                        warning("Invalid value for alpha - set alpha to 0.4")
                        alpha = 0.4
                }
        }

        if(!is.null(n_max)) {
                if(is.numeric(n_max)) {
                        if(!(n_max %% 1 == 0) | (n_max < 0)){
                                warning("Invalid value for n_max - set n_max to 5")
                                n_max = 5
                        }
                }else{
                        warning("Invalid value for n_max - set n_max to 5")
                        n_max = 5
                }
        }

        if(is.null(y)) {

                country<-unique(eq_clean$COUNTRY)
                timeline<- ggplot() +
                        geom_timeline(data=eq_clean,aes_string(x="DATE",size=size,color=color),alpha=alpha) +
                        theme_classic() +
                        theme(legend.position="bottom",axis.line.y=element_blank(),
                              axis.line.x=element_line(color="black", size = 1),
                              plot.caption = element_text(hjust=0.5, size=rel(1.2)),
                              plot.margin = unit(c(0.5, 1, 1, 1), "lines")) +
                        labs(caption=paste0("Earthquake Timeline for ",country)) +
                        scale_color_continuous(name="# Deaths") +
                        scale_size_continuous(name="Richter Scale value") +
                        scale_alpha_continuous(guide=FALSE)

        }else {

                country<-unique(eq_clean$COUNTRY)
                country_string<-paste(country,collapse=' & ')
                timeline<- ggplot() +
                        geom_timeline(data=eq_clean,aes_string(x="DATE",y=y,size=size,color=color),alpha=alpha) +
                        theme_classic() +
                        theme(legend.position="bottom",axis.line.y=element_blank(),
                              axis.line.x=element_line(color="black", size = 1),
                              plot.caption = element_text(hjust=0.5, size=rel(1.2)),
                              plot.margin = unit(c(0.5, 1, 1, 1), "lines"),
                              axis.title.y=element_blank(),
                              axis.ticks.y = element_blank()) +
                        labs(caption=paste0("Earthquake Timeline for ",country_string)) +
                        scale_color_continuous(name="# Deaths") +
                        guides(color = guide_legend(label.position="bottom",
                                                    label.hjust = 0.5, label.vjust = 0.5,
                                                    label.theme = element_text(angle = 45))) +
                        scale_size_continuous(name="Richter Scale value") +
                        scale_alpha_continuous(guide =FALSE )
        }



        #get timeline_label
        if(isTRUE(timeline_label)) {

                if(is.null(y)) {
                        timeline_label<-geom_timeline_label(data=eq_clean,aes_string(x="DATE",label="LOCATION_NAME"),n_max=n_max)
                }else{
                        timeline_label<-geom_timeline_label(data=eq_clean,aes_string(x="DATE",y=y,label="LOCATION_NAME"),n_max=n_max)
                }

                print(timeline + timeline_label)

        }else{

                print(timeline)

        }

}

