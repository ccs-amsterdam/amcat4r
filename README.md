
<!-- README.md is generated from README.Rmd. Please edit that file -->

# amcat4r: R Bindings for the AmCAT4 API

The goal of amcat4r is to provide easy access to
[`amcat4`](https://github.com/ccs-amsterdam/amcat4) from R. Learn more
about AmCAT and amcat4r at <https://amcat-book.netlify.app/>.

## Installation

You can install the development version of amcat4r from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("ccs-amsterdam/amcat4r")
```

# Usage

Before you can use AmCAT, you need to login:

```{r}
amcat_login("https://amcat4.labs.vu.nl/amcat")
```

## Querying

Some examples of how to query an index:

```{r}
query_documents(INDEX)
query_documents(INDEX, fields=list("publisher", "text"))
query_documents(INDEX, filters = list(publisher="Guardian"))
query_documents(INDEX, filters = list(date=list(lte="2024-06-01")))
query_documents(INDEX, queries = "hai*", fields = "text")
```

## Managing indices and roles

Examples of creating an index, setting the fields, and uploading documents:

```{r}
create_index(INDEX, name="Test index")

set_fields(INDEX, list(title='text', text='text', publisher='keyword', date='date'))

docs = tribble(
  ~publisher, ~date, ~title, ~text,
  "Guardian", "2025-01-01", "This is an article", "With some text",
  "NY Times", "2024-01-01", "Another article", "Haiku time: 古池や 蛙飛び込む 水の音",
)

upload_documents(INDEX, docs)
```
