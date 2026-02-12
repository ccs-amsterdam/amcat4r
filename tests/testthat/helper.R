amcat_test_server <- Sys.getenv("AMCAT4R_TEST_SERVER", unset="http://localhost/amcat")
amcat_offline = !ping(amcat_test_server)
amcat_test_index = "amcat4r_testthat_testindex"
amcat_test_apikey =  Sys.getenv("AMCAT4R_TEST_APIKEY")
amcat_test_user =  "amcat4r_testthat@example.com"
amcat_test_index_fields = list(title = "text", text = "text", date="date", keyword = "keyword", cats = "keyword")



setup_test <- function(create_index=TRUE) {
  message(amcat_test_server)
  skip_if(!ping(amcat_test_server))
  amcat_login(amcat_test_server,api_key = amcat_test_apikey)
  if (index_exists(amcat_test_index)) delete_index(amcat_test_index)
  if (create_index) {
    create_index(amcat_test_index)
    set_fields(amcat_test_index, amcat_test_index_fields)
  }
}
