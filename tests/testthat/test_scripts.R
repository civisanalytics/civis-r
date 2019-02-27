context("scripts")




test_that("test civis_script", {
  expect_equal(civis_script(1, 1),
               structure(list(id = 1, run_id = 1), class = 'civis_script'))
  expect_error(civis_script(1:2))
  expect_error(civis_script(1, 1:2))
})

test_that("script_get_fun works", {
  mock_job <- list(type = 'JobTypes::ContainerDocker')
  expect_equal(get_script_fun(mock_job, 'outputs'),
               scripts_list_containers_runs_outputs)
  mock_job <- list(type = 'JobTypes::PythonDocker')
  expect_equal(get_script_fun(mock_job, 'logs'),
               scripts_list_python3_runs_logs)
  mock_job <- list(type = 'JobTypes::ContainerDocker',
                   fromTemplateId = 1)
  expect_equal(get_script_fun(mock_job, 'outputs'),
               scripts_list_custom_runs_outputs)

  expect_error(get_script_fun(423, 'asdf'), 'outputs')
})
