library(base64)
library(magrittr)
context("utils")

# Test the right implementation of the image converter
# which first has to work without any tag specification
test_that("plot2png src", {

      img.fPath <- list.files(find.package('RTest'),recursive = T,pattern='Roche_Logo',full.names=T)[1]

      tf <- tempfile()
      base64::encode(img.fPath, tf)
      src <- sprintf("data:image/png;base64,%s", paste(readLines(tf),
              collapse = ""))

      expect_equal(
          RTest:::png2base64(img.fPath,
              img.returnAsTag = FALSE,
              img.title = "Roche Logo",
              img.width = "100px"),
          src)
    })

# Second without any defined image width
test_that("plot2png width", {

      img.fPath <- list.files(find.package('RTest'),recursive = T,pattern='Roche_Logo',full.names=T)[1]

      tf <- tempfile()
      base64::encode(img.fPath, tf)
      src <- sprintf("data:image/png;base64,%s", paste(readLines(tf),
              collapse = ""))
      value <- sprintf("<img src='%s' alt='%s' />", src, "Roche Logo")

      expect_equal(
          RTest:::png2base64(img.fPath,
              img.returnAsTag = TRUE,
              img.title = "Roche Logo",
              img.width = NULL),
          value)
    })
# NOTE: With the width it is tested in examples / vignettes

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# systemInfo.packages

test_that("systemInfo.packages error",{
      expect_error(
          RTest:::systemInfo.packages(which="error")
          )
    })

#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# normalizeDate
test_that("normalizeDate",{
      expect_equal(normalizeDate("15.September.1988",FALSE),"15.09.1988")

      expect_equal(
          normalizeDate("15.MyName.1988",FALSE,months = c("jan"="january","sep"="myname")),
          "15.02.1988")

      expect_equal(
          normalizeDate("15.sep.1988",FALSE,months = c("jan"="january","sep"="myname")),
          "15.02.1988")

      expect_equal(normalizeDate("15.MyName.1988",TRUE,
              months = c("jan"="january","sep"="myname")) %>% class(),
          "Date")

    })
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

# getFUN
test_that("getfun",{
      abc_fun <<- function(x){"abc"}
      expect_equal(
          RTest:::getfun("abc_fun"),
          "abc_fun")

    })
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------

#example_list_large
test_that("example_list_large",{

    expect_equal(
        "VALUE1",
        RTest:::example_list_large()[["NAME1"]]
        )
    expect_warning(
        RTest:::example_list_large()[["NAME1"]]

        )
    }
    )

##################################################################################################

# -   arguments_creator

##################################################################################################


context("test-adapter generic")

test_that("arguments_creator throws error",{

    expect_error(

        RTest:::arguments_creator
            (parameters_xml_definition = list("RTestData_input_data"=list())
            , input_data=NULL)
    )

    data <- '<params><RTestData_input_data name="data1" param="param1" column="c1"/></params>'
    item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))

    expect_equal(
       RTest:::arguments_creator(item,
       input_data = list(data1 = data.frame(c1 = c(1, 2, 3), c2 = c(4, 5, 6)))
       ),
       list(param1 = c(1, 2, 3))
    )

    data <- '<params><RTestData_input_data name="data1" param="param1" /></params>'
    item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))

    expect_equal(
       RTest:::arguments_creator(item,
       input_data = list(data1 = data.frame(c1 = c(1, 2, 3), c2 = c(4, 5, 6)))
       ),
       list(param1 = data.frame(c1 = c(1, 2, 3), c2 = c(4, 5, 6)))
    )
    data <- '<params>
      <RTestData_input_data name="data1" param="param1" />
      <var value="2" type="numeric" />
    </params>'
    item <- XML::xmlRoot(XML::xmlParse(data,asText=TRUE))

    expect_equal(
       RTest:::arguments_creator(item,
       input_data = list(data1 = data.frame(c1 = c(1, 2, 3), c2 = c(4, 5, 6)))
       ),
       list(param1 = data.frame(c1 = c(1, 2, 3), c2 = c(4, 5, 6)), var = 2)
    )

})

##################################################################################################

# -   get_existence_of_fun

##################################################################################################
test_that("get_existence_of_fun",{
  expect_error(
      RTest:::get_existence_of_fun("myfun","testthat")
  )

  expect_equal(
      RTest:::get_existence_of_fun("normalise_names","testthat"),
      "package"
  )
})

##################################################################################################

# -   RTM

##################################################################################################

test_that("RTM util", {

  directory_with_tests <- list.dirs(find.package('RTest'),recursive=TRUE) %>%
      grep(pattern="xml-templates",value=TRUE)

 expect_equal("data.frame", class(RTest::RTest.getRTM(
      testcase.directory = directory_with_tests[1],
      open=FALSE,
      f.pattern = "RTest_TC-generic.xml"
  )))
})
