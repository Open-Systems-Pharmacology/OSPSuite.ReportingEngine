# ExcelMessaging

R6 class for tracking and printing messages for the Excel template

## Public fields

- `type`:

  "warnings" or "errors" (could potentially include other kind of
  information)

- `messages`:

  list of messages included in each script section

## Methods

### Public methods

- [`ExcelMessaging$new()`](#method-ExcelMessaging-new)

- [`ExcelMessaging$displayMessage()`](#method-ExcelMessaging-displayMessage)

- [`ExcelMessaging$getMessageHeader()`](#method-ExcelMessaging-getMessageHeader)

- [`ExcelMessaging$getNoIssueMessage()`](#method-ExcelMessaging-getNoIssueMessage)

------------------------------------------------------------------------

### Method `new()`

Initialize an `ExcelMessaging` object

#### Usage

    ExcelMessaging$new(type)

#### Arguments

- `type`:

  "warnings" or "errors"

#### Returns

A new `ExcelMessaging` object

------------------------------------------------------------------------

### Method `displayMessage()`

display the messages stored in messages

#### Usage

    ExcelMessaging$displayMessage()

#### Returns

Messages content

------------------------------------------------------------------------

### Method `getMessageHeader()`

Get message header

#### Usage

    ExcelMessaging$getMessageHeader(section)

#### Arguments

- `section`:

  name of R script section

------------------------------------------------------------------------

### Method `getNoIssueMessage()`

Get message displaying no issue identified

#### Usage

    ExcelMessaging$getNoIssueMessage()
