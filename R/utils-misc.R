# It doesn't make sense for a reactive to trigger when its value has not changed,
# given the fact that every reactive caches its value.
# `reactiveVal` does not suffer from that design mistake, so we can use it to
# filter spureous reactive invalidations.
# We have to apply this treatment judiciously because it makes an extra copy of
# the value returned by the reactive it wraps around, which may prove expensive.
#
# TODO: test this when r() returns a silent error, this may crash the app, or the error value will not be propagated
# though rv()
trigger_only_on_change <- function(r) {
  rv <- shiny::reactiveVal()
  shiny::observe(rv(r()))
  rv
}
