f_rand <- function(job_id, ...) {
  x <- runif(1)
  if (x < .9) {
    return(list(state = "succeeded", job_id = job_id, args = list(...)))
  } else {
    return(list(state = "partying instead", job_id = job_id, args = list(...)))
  }
}


set.seed(2)
x <- await_all(f_rand, .x = 1:2, .y = 3:4)
print(x)
# expect_is(x, "list")
expect_equal(sapply(x, get_status), rep("succeeded", 2))
expect_equal(sapply(x, function(x) x$args), list(c(1, 3), c(2, 4)))















# ids <- c(23820523, 23820523)
# runs <- c(130390319, 130390319)
# result <- await_all(scripts_get_r_runs, .x = ids, .y = runs)
# print(result)


# test_function <- function(x) {
#   if (x == 0) list(state = "success", y = 2) else list(state = "fail", y = NULL)
# }
#
# # expect_silent
# r <- await(f = test_function, .x = 0, .status_key = "state",
#                          .success_states = c("success", "fail"))
#
# r

# # expect = "success"
# r$state
#
# # expect = "success"
# get_status(r)
#
# # expect = 2
# r$y
#
# r <- await(f, x = 1, .status_key = "state", .success_states = c("success", "fail"))
# expect_equal(r$state, "fail")
# expect_equal(get_status(r), "fail")
# expect_null(r$y)
