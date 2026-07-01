# Local storage usage:
# 1. Add each persisted input to LOCAL_STORAGE_INPUTS as a list with:
#    id = the Shiny input id, default = the fallback value, update = the
#    matching Shiny update function, and transform = a parser for stored values.
# 2. In the corresponding UI/server file, read the restored value with
#    local_storage_get_value(session, "input_id", default, transform) when
#    setting the initial value/selected option.
# 3. init_local_storage(input, session) is called from server.R and observes
#    every entry in LOCAL_STORAGE_INPUTS.
# Stored browser keys are prefixed as "inzight-lite:<input id>".

LOCAL_STORAGE_PREFIX = "inzight-lite"

LOCAL_STORAGE_INPUTS = list(
  list(
    id = "visualize_plot_height",
    default = 60,
    update = updateSliderInput,
    transform = as.numeric
  ),
  list(
    id = "visualize_plot_width",
    default = 100,
    update = updateSliderInput,
    transform = as.numeric
  ),
  list(
    id = "select.bg1",
    default = "grey93",
    update = updateSelectInput,
    transform = as.character
  ),
  list(
    id = "ci.width",
    default = 95,
    update = updateNumericInput,
    transform = as.numeric
  ),
  list(
    id = "global.sig.level.inf",
    default = 4,
    update = updateNumericInput,
    transform = as.numeric
  ),
  list(
    id = "global.p.val",
    default = 0.0001,
    update = updateNumericInput,
    transform = as.numeric
  )
)

local_storage_key = function(id) {
  paste(LOCAL_STORAGE_PREFIX, id, sep = ":")
}

local_storage_input_id = function(id) {
  paste0(".local_storage_", id)
}

local_storage_json = function(value) {
  jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
}

local_storage_equal = function(x, y) {
  isTRUE(all.equal(x, y, check.attributes = FALSE))
}

local_storage_update_input = function(update, session, id, value) {
  update_args = names(formals(update))

  if ("value" %in% update_args) {
    update(session, id, value = value)
  } else if ("selected" %in% update_args) {
    update(session, id, selected = value)
  } else {
    update(session, id, value)
  }
}

local_storage_values = function(session) {
  if (is.null(session$userData$local_storage_values)) {
    # Use new.env() instead of list() so values[[id]] = value updates the
    # session storage directly. With a list, we would need to assign it back?
    session$userData$local_storage_values = new.env(parent = emptyenv())
  }

  session$userData$local_storage_values
}

local_storage_set_value = function(session, id, value) {
  values = local_storage_values(session)
  values[[id]] = value

  invisible(value)
}

local_storage_get_value = function(
    session,
    id,
    default = NULL,
    transform = identity
) {
  values = session$userData$local_storage_values

  value = if (!is.null(values) && exists(id, values, inherits = FALSE)) {
    values[[id]]
  } else {
    default
  }

  if (is.null(value)) {
    value = default
  }

  transform(value)
}

local_storage_read = function(id, default) {
  shinyjs::runjs(sprintf(
    "
    try {
      var value = %s;
      var stored = window.localStorage.getItem(%s);

      if (stored !== null) {
        value = JSON.parse(stored);
      }

      Shiny.setInputValue(%s, value, { priority: 'event' });
    } catch (error) {
      console.warn('Unable to read iNZight Lite local storage value', error);
      Shiny.setInputValue(%s, %s, { priority: 'event' });
    }
    ",
    local_storage_json(default),
    local_storage_json(local_storage_key(id)),
    local_storage_json(local_storage_input_id(id)),
    local_storage_json(local_storage_input_id(id)),
    local_storage_json(default)
  ))
}

local_storage_write = function(id, value) {
  shinyjs::runjs(sprintf(
    "
    try {
      window.localStorage.setItem(%s, JSON.stringify(%s));
    } catch (error) {
      console.warn('Unable to write iNZight Lite local storage value', error);
    }
    ",
    local_storage_json(local_storage_key(id)),
    local_storage_json(value)
  ))
}

local_storage_observe_input = function(
  input,
  session,
  id,
  default = NULL,
  update = NULL,
  transform = identity
) {
  loaded = reactiveVal(FALSE)
  ready_to_write = reactiveVal(FALSE)
  restored_value = reactiveVal(NULL)
  local_storage_read(id, default)

  observeEvent(input[[local_storage_input_id(id)]], {
    value = transform(input[[local_storage_input_id(id)]])
    local_storage_set_value(session, id, value)
    restored_value(value)
    ready_to_write(!is.null(input[[id]]) && local_storage_equal(input[[id]], value))

    if (!is.null(update)) {
      local_storage_update_input(update, session, id, value)
    }

    loaded(TRUE)
  }, once = TRUE)

  observeEvent(input[[id]], {
    if (!isTRUE(loaded())) {
      return()
    }

    if (!isTRUE(ready_to_write())) {
      if (local_storage_equal(input[[id]], restored_value())) {
        ready_to_write(TRUE)
      }

      return()
    }

    local_storage_set_value(session, id, input[[id]])
    local_storage_write(id, input[[id]])
  }, ignoreInit = TRUE)
}

init_local_storage = function(input, session, inputs = LOCAL_STORAGE_INPUTS) {
  invisible(lapply(inputs, function(x) {
    local_storage_observe_input(
      input = input,
      session = session,
      id = x$id,
      default = x$default,
      update = x$update,
      transform = x$transform
    )
  }))
}
