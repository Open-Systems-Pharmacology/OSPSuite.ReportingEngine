# Logging

R6 class managing logging

## Public fields

- `messages`:

  Array of messages to log. Each element of `messages` is a list that
  includes `time`, `type` and `text` of message to log.

- `folder`:

  Folder where log files are saved

## Methods

### Public methods

- [`Logging$info()`](#method-Logging-info)

- [`Logging$error()`](#method-Logging-error)

- [`Logging$debug()`](#method-Logging-debug)

- [`Logging$textToMessage()`](#method-Logging-textToMessage)

- [`Logging$addMessage()`](#method-Logging-addMessage)

- [`Logging$showMessages()`](#method-Logging-showMessages)

- [`Logging$printMessage()`](#method-Logging-printMessage)

- [`Logging$saveMessage()`](#method-Logging-saveMessage)

- [`Logging$writeAsJson()`](#method-Logging-writeAsJson)

- [`Logging$write()`](#method-Logging-write)

- [`Logging$reset()`](#method-Logging-reset)

------------------------------------------------------------------------

### Method `info()`

Add an `Info` message to log

#### Usage

    Logging$info(text, display = NULL)

#### Arguments

- `text`:

  content of the `Info` message

- `display`:

  Logical setting if message is displayed on console

------------------------------------------------------------------------

### Method `error()`

Add an `Error` message to log

#### Usage

    Logging$error(text, display = NULL)

#### Arguments

- `text`:

  content of the `Error` message

- `display`:

  Logical setting if message is displayed on console

------------------------------------------------------------------------

### Method [`debug()`](https://rdrr.io/r/base/debug.html)

Add a `Debug` message to log

#### Usage

    Logging$debug(text, display = NULL)

#### Arguments

- `text`:

  content of the `Debug` message

- `display`:

  Logical setting if message is displayed on console

------------------------------------------------------------------------

### Method `textToMessage()`

Add meta data to `text` to create an informative message to log with a
flag for its `logType` (`"Info"`, `"Error"`, or `"Debug"`)

#### Usage

    Logging$textToMessage(text, logType)

#### Arguments

- `text`:

  content of the message

- `logType`:

  type of message (`"Info"`, `"Error"`, or `"Debug"`)

------------------------------------------------------------------------

### Method `addMessage()`

Add a message to the list of logged `messages`

#### Usage

    Logging$addMessage(message)

#### Arguments

- `message`:

  content of the message

------------------------------------------------------------------------

### Method `showMessages()`

Display logged `messages` as a data.frame Can be filtered according to
`logTypes` (`"Info"`, `"Error"`, and/or `"Debug"`)

#### Usage

    Logging$showMessages(logTypes = LogTypes)

#### Arguments

- `logTypes`:

  types of message (`"Info"`, `"Error"`, or `"Debug"`)

------------------------------------------------------------------------

### Method `printMessage()`

Print the log messages on R/RStudio console

#### Usage

    Logging$printMessage(message, display = NULL)

#### Arguments

- `message`:

  Message to print

- `display`:

  Logical setting if message is displayed on console

------------------------------------------------------------------------

### Method `saveMessage()`

Write a message in its log file

#### Usage

    Logging$saveMessage(message)

#### Arguments

- `message`:

  Message to print

------------------------------------------------------------------------

### Method `writeAsJson()`

Write the log messages in a json log file

#### Usage

    Logging$writeAsJson(fileName)

#### Arguments

- `fileName`:

  Name of json log file

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write the log messages in txt log files

#### Usage

    Logging$write()

------------------------------------------------------------------------

### Method `reset()`

Reset/empty log messages

#### Usage

    Logging$reset(folder = NULL)

#### Arguments

- `folder`:

  Folder where logs are saved
