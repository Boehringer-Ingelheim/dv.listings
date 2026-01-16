local({
  store_path <- file.path(tempdir(), "file_system_checks")
  dir.create(store_path)
  
  dir.create(file.path(store_path, 'dummy_dir'))
  
  dummy_file_contents <- raw(1)
  writeBin(con = file.path(store_path, 'dummy_file'), object = dummy_file_contents)
  dummy_nested_file_contents <- raw(2)
  writeBin(con = file.path(store_path, 'dummy_dir', 'dummy_nested_file'), object = dummy_nested_file_contents)
  
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE, after = FALSE)
 
  callback_count <- 0L
  dummy_callback <- function(v){
    callback_count <<- callback_count + 1L
    expect_equal(v, callback_count)
  }
  
  fs_client <- fs_init(store_path)
  fs_state <- fs_client[["state"]]
  fs_contents <- fs_state[["contents"]]
  fs_client[["list"]](callback = dummy_callback)
  
  listing <- fs_client[["state"]][["listing"]]
  
  fnames <- rownames(listing[!listing[["isdir"]],])
  fs_client[["read"]](fnames, callback = dummy_callback)
  
  expect_length(fs_state[['error']], 0)
  expect_identical(names(fs_contents), c("dummy_file", "dummy_dir/dummy_nested_file"))
  expect_identical(fs_contents[["dummy_file"]], dummy_file_contents)
  expect_identical(fs_contents[["dummy_dir/dummy_nested_file"]], dummy_nested_file_contents)
  
  expect_identical(callback_count, 2L)
})

local({
  store_path <- file.path(tempdir(), "file_system_checks")
  dir.create(store_path)
  on.exit(unlink(store_path, recursive = TRUE), add = TRUE, after = FALSE)
 
  fs_client <- fs_init(store_path)
  fs_client[["list"]]()
  write <- fs_client[["write"]]
  read <- fs_client[["read"]]
  state <- fs_client[["state"]]
  
  tests_js <- rawToChar(readBin('file_tests.js', what = raw(0), n = file.size('file_tests.js')))
  test_json <- sub(';\\s+$', '', sub("^[^\\[]+", '', tests_js))
  
  tests <- jsonlite::parse_json(test_json)
  
  for(test in tests){
    path <- tempfile()
    fname <- basename(tempfile())
    for(action in test[["actions"]]){
      write(path = fname, contents = charToRaw(action[["contents"]]), offset = action[["offset"]])
    }
    read(fname)
    raw_contents <- state[["contents"]][[fname]]
    
    expect_identical(raw_contents, charToRaw(test[["result"]]), info = test[["name"]])
  }
})
