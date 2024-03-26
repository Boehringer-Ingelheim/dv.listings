# THIS FILE IS NAMED aaa_preface.R SO IT IS LOADED BEFORE ALL OTHER FILES
# DO NOT CHANGE ITS NAME. IT MUST BE THE FIRST ONE ALPHABETICALLY.

#' Build a collection of named constants
#'
#' @param ... Named parameters to be collected as constants
#' @details
#' Shiny uses strings as IDs to link UI and server elements. E.g:
#'   foo_UI(id = ns("foo")) ...
#'   foo_server(id = "foo")
#'
#' This pattern makes it easy for programmers to fall on the trap of modifying one instance of the string literal "foo"
#' without modifying the rest and be unaware of the problem until a bug is hit. It's also easy to mistakes uses of "foo"
#' as an identifier from other uses (text labels, ...) when, as it's often the case, the parameter is not explicitly
#' named.
#' One easy fix consists in using global variables instead of plain string literals. In the case of the previous
#' example, that would mean:
#'   ID_FOO <- "foo"
#'   foo_UI(ns(ID_FOO)) ...
#'   foo_server(ID_FOO)
#'
#' That simple addition makes the purpose of ID_FOO clear and also fails gracefully when not all ID_FOO instances are
#' updated synchronously along a codebase. It has the drawback of polluting the global namespace with identifier
#' variables. That's easily solved by creating a container of constants, which is the purpose of this pack_of_constants
#' alias.
#'  ID <- pack_of_constants(FOO = "foo", BAR = "bar")
#'  ID$FOO
#'  "foo"
#'  ID$BA
#'  NULL
#'
#' @keywords internal
#'
#' The simpler container we could use for this purpose is a plain named list, but that data structure has the
#' undesirable effect of partial matching. Environments, on the other hand avoid that pitfall.
pack_of_constants <- function(...) list2env(list(...))
