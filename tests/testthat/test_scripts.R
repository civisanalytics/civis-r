context("scripts")

test_that('fetch_output_file_ids returns named list of file_ids', {
  mock_output <- list(list(name = 'fake_name', objectId = 1),
                      list(name = 'asdf', objectId = 2))
  with_mock(
    `civis::jobs_get` = function(...)  list(type = 'JobTypes::ContainerDocker'),
    `civis::scripts_list_containers_runs_outputs` = function(...) mock_output,
    expect_equal(fetch_output_file_ids(civis_script(1,1), NULL),
                 list(fake_name = 1, asdf = 2)),
    expect_equal(fetch_output_file_ids(civis_script(1,1), 'fake'),
                 list(fake_name = 1))
  )
})

test_that("fetch_output dispatches correct function", {
  mock_output <- list(list(name = 'fake_name'), list(name = 'asdf'))
  with_mock(
    `civis::jobs_get` = function(...)  list(type = 'JobTypes::ContainerDocker'),
    `civis::scripts_list_containers_runs_outputs` = function(...) mock_output,
    expect_equal(fetch_output(civis_script(1,1)),
                 mock_output),
    expect_equal(fetch_output(civis_script(1,1), regex = 'fake'),
                 mock_output[1])
  )
})

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
