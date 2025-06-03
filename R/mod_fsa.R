fsa_init <- function(input_id, session = shiny::getDefaultReactiveDomain()) {

  checkmate::assert_string(input_id, min.chars = 1)

  ns <- session[["ns"]]

  attach_id <- paste0(input_id, "_attach")
  list_id <- paste0(input_id, "_list")
  read_id <- paste0(input_id, "_read")
  write_id <- paste0(input_id, "_write")
  append_id <- paste0(input_id, "_append")

  .attach <- function() {
    session$sendCustomMessage("dv_fsa_attach", list(status_input_id = ns(attach_id)))
  }

  .list <- function() {
    session$sendCustomMessage("dv_fsa_list", list(status_input_id = ns(list_id)))
  }

  .read <- function(file_name, contents) {
    session$sendCustomMessage(
      "dv_fsa_read",
      list(status_input_id = ns(read_id), file_name = file_name))
  }

  .write <- function(file_name, contents) {
    session$sendCustomMessage(
      "dv_fsa_write",
      list(status_input_id = ns(write_id), file_name = file_name, contents = contents))
  }

  .append <- function(file_name, contents) {
    session$sendCustomMessage(
      "dv_fsa_append",
      list(status_input_id = ns(read_id), file_name = file_name, contents = contents))
  }

  .show_overlay <- function(message) {session$sendCustomMessage("dv_fsa_show_overlay", list(message = message))}

  .hide_overlay <- function() {session$sendCustomMessage("dv_fsa_hide_overlay", list())}

  res <- list(
    attach = list(f = .attach, id = attach_id),
    list = list(f = .list, id = list_id),
    write = list(f = .write, id = write_id),
    read = list(f = .read, id = read_id),
    append = list(f = .append, id = append_id)
  )
  return(res)
}
