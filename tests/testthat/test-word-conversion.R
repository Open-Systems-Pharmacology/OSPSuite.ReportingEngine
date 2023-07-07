# Expose unexported function for test
translateForWord <- ospsuite.reportingengine:::translateForWord

test_that("html subscript and super script tags are converted", {
  expect_equal(
    translateForWord("Superscript example 95<sup>th</sup> percentile"),
    "Superscript example 95^th^ percentile"
  )
  expect_equal(
    translateForWord("Subscript example H<sub>2</sub>0"),
    "Subscript example H~2~0"
  )
})

test_that("html alignment tags are translated into inline openxml", {
  mdTable <- paste0(
    "|",
    c(
      paste(c("left", "right", "center", "justify"), "column", collapse = "|"),
      "---|---|---",
      paste0('<div align="', c("left", "right", "center", "justify"), '">test</div>', collapse = "|")
    ),
    "|"
  )
  wordTable <- paste0(
    "|",
    c(
      paste(c("left", "right", "center", "justify"), "column", collapse = "|"),
      "---|---|---",
      paste0('`<w:pPr><w:jc w:val="', c("left", "right", "center", "both"), '"/></w:pPr>`{=openxml}test', collapse = "|")
    ),
    "|"
  )

  expect_equal(
    translateForWord(mdTable),
    wordTable
  )
})

test_that("html anchor tags are translated into inline openxml bookmarks", {
  expect_equal(
    translateForWord('<a id="my-bookmark"></a>'),
    '`<w:bookmarkStart w:id="my-bookmark" w:name="my-bookmark"/><w:bookmarkEnd w:id="my-bookmark"/>`{=openxml}'
  )
  expect_equal(
    translateForWord('Tralala before <a id="my-bookmark"></a> tralala after'),
    paste0(
      "Tralala before ",
      " tralala after",
      '`<w:bookmarkStart w:id="my-bookmark" w:name="my-bookmark"/><w:bookmarkEnd w:id="my-bookmark"/>`{=openxml}'
    )
  )
})

test_that("html anchor figure and table tags are translated with pagebreaks", {
  expect_equal(
    translateForWord('<a id="figure-1"></a>'),
    c(
      "```{=openxml}",
      "<w:p><w:r><w:br w:type=\"page\"/></w:r></w:p>",
      "```",
      "",
      '`<w:bookmarkStart w:id="figure-1" w:name="figure-1"/><w:bookmarkEnd w:id="figure-1"/>`{=openxml}'
    )
  )
  expect_equal(
    translateForWord('<a id="table-1"></a>'),
    c(
      "```{=openxml}",
      "<w:p><w:r><w:br w:type=\"page\"/></w:r></w:p>",
      "```",
      "",
      '`<w:bookmarkStart w:id="table-1" w:name="table-1"/><w:bookmarkEnd w:id="table-1"/>`{=openxml}'
    )
  )
  expect_equal(
    translateForWord('Tralala before <a id="figure-1"></a> tralala after'),
    paste0(
      "Tralala before ",
      " tralala after",
      '`<w:bookmarkStart w:id="figure-1" w:name="figure-1"/><w:bookmarkEnd w:id="figure-1"/>`{=openxml}'
    )
  )
  expect_equal(
    translateForWord('Tralala before <a id="table-1"></a> tralala after'),
    paste0(
      "Tralala before ",
      " tralala after",
      '`<w:bookmarkStart w:id="table-1" w:name="table-1"/><w:bookmarkEnd w:id="table-1"/>`{=openxml}'
    )
  )
})
