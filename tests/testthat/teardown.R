
# The tests use this index, so let's delete it to avoid clutter
# if (index_exists(amcat_test_index)) delete_index(amcat_test_index)

amcat_login(amcat_test_server, api_key = amcat_test_apikey, test_login = FALSE, cache=2)
if (amcat_test_user %in% list_users()) delete_user(amcat_test_user)
