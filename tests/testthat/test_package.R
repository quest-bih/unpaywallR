doi_1 <- "10.1103/physreve.88.012814"
doi_2 <- "10.1186/s12864-016-2566-9"
doi_3 <- "10.1371/journal.pone.0153190"
email <- "test@email.com"

locations <- .to_oaDOI(doi_3, email) |> 
  readLines() |>
  jsonlite::fromJSON() |> 
  purrr::pluck("oa_locations")

test_that("PDF_links",
          {
            expect_true(dois_pdf_link(doi_1, email)[1,2] ==
                          "https://arxiv.org/pdf/1304.0473")
          })

test_that("OA colors",
          {
            expect_equivalent(dois_OA_colors(c(doi_2), email)[1,2], "gold")
            expect_equivalent(dois_OA_colors(c(doi_1), email)[1,2], "green")
            expect_equivalent(.get_loc_article_color(locations |> dplyr::slice(1), TRUE, TRUE), "green") 
            expect_equivalent(.get_loc_article_color(locations |> dplyr::slice(2), TRUE, TRUE), "gold") 
          })

test_that("OA hierarchy",
          {
            expect_true(dois_OA_colors(
              c(doi_1), email,
              color_hierarchy = c("gold", "hybrid", "green",
                                  "bronze", "closed"))[1,2] == "green")
            # expect_true(dois_OA_colors(
            #   c(doi_1), email,
            #   color_hierarchy = c("gold", "bronze", "hybrid", "green",
            #                       "closed"))[1,2] == "bronze")
            expect_true(dois_OA_colors(
              c(doi_2), email,
              color_hierarchy = c("gold", "hybrid", "green", "bronze",
                                  "closed"))[1,2] == "gold")
            expect_true(dois_OA_colors(
              c(doi_2), email,
              color_hierarchy = c("green", "gold", "hybrid", "bronze",
                                  "closed"))[1,2] == "green")
            expect_true(dois_OA_colors(
              c(doi_3), email,
              color_hierarchy = c("green", "gold", "hybrid", "bronze",
                                  "closed"))[1,2] == "green")
            expect_true(dois_OA_colors(
              c(doi_3), email,
              color_hierarchy = c("gold", "hybrid", "green", "bronze",
                                  "closed"))[1,2] == "gold")
          })
