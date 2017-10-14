#tests for file reading and data processing

test_that("filename is valid",{
        expect_that(eq_data_read("file_not_here"),throws_error())
})

test_that("eq_data_read returns a tbl_df", {
        filename <- system.file("extdata","data.gz",package="NOAAA")
        expect_is(eq_data_read(filename), "tbl_df")
})

test_that("required column present in data",{
        filename <- system.file("extdata","data.gz",package="NOAAA")
        eq_raw2 <- eq_data_read(filename) %>% dplyr::select(- LOCATION_NAME)

        expect_that(eq_clean_data(eq_raw2),throws_error())
        expect_that(eq_location_clean(eq_raw2),throws_error())
})

test_that("eq_clean_data generates correct column types", {
        filename <- system.file("extdata","data.gz",package="NOAAA")
        eq_clean <- eq_data_read(filename) %>% eq_clean_data()
        expect_is(eq_clean$DATE,"Date")
        expect_is(eq_clean$LATITUDE,"numeric")
        expect_is(eq_clean$LONGITUDE, "numeric")
        expect_is(eq_clean$EQ_PRIMARY, "numeric")
        expect_is(eq_clean$DEATHS, "numeric")
})

test_that("eq_location_clean changes LOCATION_NAME", {
        filename <- system.file("extdata","data.gz",package="NOAAA")
        eq_raw <- eq_data_read(filename)
        eq_loc_clean <- eq_location_clean(eq_raw)
        expect_that(any(eq_raw$LOCATION_NAME != eq_loc_clean$LOCATION_NAME),is_true())
})

#tests for timeline visualization

test_that("values for timeline arguments are correct",{
        filename <- system.file("extdata","data.gz",package="NOAAA")
        sample_USA <- eq_data_read(filename) %>% eq_clean_data() %>%
                dplyr::filter((COUNTRY=="USA") & lubridate::year(DATE) >= 2000)

        #call to eq_time
        expect_that(eq_time(eq_clean=sample_USA,y="LOCATION_NAME"),throws_error())
        expect_that(eq_time(eq_clean=sample_USA,color="LOCATION_NAME"),throws_error())
        expect_that(eq_time(eq_clean=sample_USA,size="LOCATION_NAME"),throws_error())
        expect_that(eq_time(eq_clean=sample_USA,timeline_label="MAYBE"),gives_warning())
        expect_that(eq_time(eq_clean=sample_USA,timeline_label=TRUE,n_max="no"),gives_warning())
        expect_that(eq_time(eq_clean=sample_USA,alpha=10),gives_warning())
        expect_that(eq_time(eq_clean=sample_USA,alpha="no"),gives_warning())

        #no DATE
        sample_USA2 <- sample_USA %>% dplyr::select(- DATE)
        expect_that(eq_time(sample_USA2),throws_error())

})


test_that("visualizations are created", {
        filename <- system.file("extdata","data.gz",package="NOAAA")
        sample_USA <- eq_data_read(filename) %>% eq_clean_data() %>%
                dplyr::filter((COUNTRY=="USA") & lubridate::year(DATE) >= 2000)

        #geom_timeline
        timeline <- ggplot(sample_USA) +
                geom_timeline(aes(x=DATE,size=EQ_PRIMARY,color=DEATHS),alpha=0.5)

        expect_that(timeline, is_a("ggplot"))
        expect_that(timeline$layers[[1]]$geom,is_a("GeomTimeLine"))

        #geom_timeline_label
        timeline_label <- ggplot(data = sample_USA, aes(x=DATE)) + geom_timeline() +
                geom_timeline_label(data=sample_USA,aes(label=LOCATION_NAME),n_max=8)

        expect_that(timeline_label, is_a("ggplot"))
        expect_that(timeline_label$layers[[2]]$geom,is_a("GeomTimeLineLabel"))
})

#tests for map visualization

test_that("eq_map columns and annot_col are present",{
        filename <- system.file("extdata","data.gz",package="NOAAA")
        sample_MEXICO <- eq_data_read(filename) %>% eq_clean_data() %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000)

        expect_that(eq_map(eq_clean=sample_MEXICO,annot_col="hello"),gives_warning())

        sample_MEXICO2 <- sample_MEXICO %>% dplyr::select(- LATITUDE)
        expect_that(eq_map(eq_clean=sample_MEXICO2,annot_col="DATE"),throws_error())

        sample_MEXICO2 <- sample_MEXICO %>% dplyr::select(- LOCATION_NAME)
        expect_that(eq_create_label(sample_MEXICO2),throws_error())

})

test_that("eq_create_label creates a text column",{
        filename <- system.file("extdata","data.gz",package="NOAAA")
        sample_MEXICO_popup <- eq_data_read(filename) %>% eq_clean_data() %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                dplyr::mutate(popup_text = eq_create_label(.))

        expect_that(sample_MEXICO_popup$popup_text, is_a("character"))
})


test_that("eq_map creates leaflet visualization", {
        filename<-system.file("extdata","data.gz",package="NOAAA")
        sample_MEXICO_map <- eq_data_read(filename) %>% eq_clean_data() %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                eq_map(annot_col = "DATE")

        expect_that(sample_MEXICO_map, is_a("leaflet"))
})
