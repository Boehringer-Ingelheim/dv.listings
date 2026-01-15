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
  
  OFFSET_APPEND = -1
 
  # TODO: Move this to JSON so that the JS tester can also execute it 
  tests <- list(
    list(name = "Write 0 bytes", 
         actions = list(
           list(contents = raw(0), offset = 0L)
         ),
         result = ""),
    list(name = "Append 0 bytes", 
         actions = list(
           list(contents = raw(0), offset = OFFSET_APPEND)
         ),
         result = ""),
    list(name = "Append 1 byte", 
         actions = list(
           list(contents = charToRaw("H"), offset = OFFSET_APPEND)
         ),
         result = "H"),
    list(name = "Append 2 bytes",
         actions = list(
           list(contents = charToRaw("He"), offset = OFFSET_APPEND)
         ),
         result = "He"),
    list(name = "Append 2 bytes, then three bytes",
         actions = list(
           list(contents = charToRaw("He"), offset = OFFSET_APPEND),
           list(contents = charToRaw("llo"), offset = OFFSET_APPEND)
         ),
         result = "Hello"),
    list(name = "Append 2 bytes, then three bytes, but overwriting one",
         actions = list(
           list(contents = charToRaw("Hello"), offset = OFFSET_APPEND),
           list(contents = charToRaw(" "), offset = 4)
         ),
         result = "Hell ")
  )
  
  for(test in tests){
    path <- tempfile()
    fname <- basename(tempfile())
    for(action in test[["actions"]]){
      write(path = fname, contents = action[["contents"]], offset = action[["offset"]])
    }
    read(fname)
    raw_contents <- state[["contents"]][[fname]]
    
    hash <- tools::sha256sum(files = file.path(store_path, fname))
    expect_identical(raw_contents, charToRaw(test[["result"]]), info = test[["name"]])
  }
})
