# extensionList ------------------------------------------------------------
extensionList <- R6Class(
  "extensionList",
  public = list(
    initialize = function(x) {
      private$parent <- x
      self$extensions_data <- private$get_extensions_df()
    },
    remove_extension = function(name) {
      if (!name %in% self$extensions_data$name) {
        return(
          message(
            sprintf("No extension with name %s found.", name)
          )
        )
      }

      uri <- self$extensions_data$uri[name %in% self$extensions_data$name]

      private$parent$get() |>
        xml_find_first(sprintf("//p:ext[@uri='%s']", uri)) |>
        xml_remove()

      self$extensions_data <- private$get_extensions_df()

      message(
        sprintf("Removed extension %s.", name)
      )
    },
    add_extension = function(name, ext, ...) {
      if (name %in% self$extensions_data$name) {
        return(
          message(
            sprintf("There is already an extension with name %s.", name)
          )
        )
      }

      if (is.null(private$extensions)) {
        private$parent$get() |>
          xml_add_child("p:extLst")

        private$extensions <- list()
      }

      obj <- ext$new(self, ...)

      self$extensions_data <- rbind(
        self$extensions_data,
        obj$get_meta()
      )

      private$extensions <- c(
        private$extensions,
        setNames(list(obj), name)
      )

      self
    },
    get_extension = function(name) {
      private$extensions[[name]]
    },
    get = function() {
      xml_find_all(private$parent$get(), "//p:extLst")
    },
    extensions_data = NULL
  ),
  private = list(
    parent = NULL,
    extensions = NULL,
    get_extensions_df = function() {
      ext <- xml_find_all(self$get(), "//p:ext")

      if (length(ext) == 0) {
        return(
          data.frame(
            name = character(0),
            uri = character(0),
            ns = character(0),
            prefix = character(0)
          )
        )
      }

      ns_list <- sapply(
        xml_attrs(xml_children(ext)),
        function(x) x[grep("^xmlns:", names(x), value = TRUE)]
      )

      data.frame(
        name = xml_name(xml_children(ext)),
        uri = xml_attr(ext, "uri"),
        ns = unname(ns_list),
        prefix = gsub("^xmlns:", "", names(ns_list))
      )
    }
  )
)

# extension ------------------------------------------------------------
Extension <- R6Class(
  "Extension",
  public = list(
    initialize = function(parent,
                          name = NULL,
                          uri = NULL,
                          ns = NULL,
                          prefix = NULL,
                          child = NULL) {
      private$name <- name
      private$uri <- uri
    },
    get_meta = function() {
      data.frame(
        name = private$name,
        uri = private$uri,
        ns = private$ns,
        prefix = private$prefix
      )
    }
  ),
  private = list(
    name = NULL,
    uri = NULL,
    ns = NULL,
    prefix = NULL
  )
)

# convenience functions ----
extension_summary <- function(x) {
  x$presentation$extension_list$extensions_data
}
