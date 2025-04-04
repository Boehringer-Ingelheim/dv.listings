# TODO: Tag as shared snippet (also used in dv.bookman)
DTH <- local({
  # _DT_ _H_elpers
  set_column_heading_hover_info <- function(title, text) {
    # Appends a 🛈 sign next to the title that shows a hover-on `text` message.
    # Requires DT::datatable(escape = FALSE) 
    paste(
      title,
      htmltools::tags$i(
        class = "glyphicon glyphicon-info-sign",
        style = "color:#0072B2;",
        title = text
      ) |> as.character()
    )
  }
  
  return(list(
    set_column_heading_hover_info = set_column_heading_hover_info
  ))
})
