#' POST a full query to the REST API for Cosmos DB.
#'
#' @param sql.what String for specifying what fields to retrieve. Typically called select condition. Defaults to *
#' @param sql.where String for specifying what filter to use on data. Typically called search condition. Defaults to empty.
#' @param max.items Number of documents to retrieve per page
#' @param continuation Logical specifying whether to try to grab a next page of results
#' @param debug.auth Logical value for getting verbose output of auth header being constructed. Defaults to false.
#' @param debug.query Logical value for getting verbose output of HTTP response, printing all headers. Defaults to false.
#' @param content.response Logical value to determine whether to retrieve full response or just the documents
#' @param content.flatten Logical value to determine whether to flatten the data frame returned. Only performed when content.response = TRUE
#' @param content.removecosmos Logical value to determine whether to remove Cosmos DB columns from the returned data frame. Only performed when content.response = TRUE
#' @return Prints status code of HTTP POST, and returns full HTTP response or just the content
#' @keywords query cosmosdb post
#' @export
#' @examples
#' cosmosQuery(sql.what = "c.contact.eloquaId", sql.where = "c.contact.eloquaId != null")

cosmosQuery <- function(sql.what = "*", sql.where = "", max.items = 100, continuation = FALSE, debug.auth = FALSE, debug.query = FALSE, content.response = TRUE, content.flatten = content.response, content.removecosmos = content.response) {

    require(digest)
    require(base64enc)
    require(httr)
    require(jsonlite)
    require(stringr)

    # Use the current time to create a proper auth header
    current.time <- Sys.time()

    # Coerce current time to proper format
    ms.date.string <- tolower(format(current.time, "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"))

    # Create POST URI for posting query to collection
    post.uri <- paste(envCosmosDB$uri, "/dbs/", envCosmosDB$dbName, "/colls/", envCosmosDB$collName, "/docs", sep = "")

    # Create the resource link and type from the environment variables
    res.link <- paste("dbs/", envCosmosDB$dbName, "/colls/", envCosmosDB$collName, sep = "")
    res.type <- "docs"

    # Create full query with function
    full.query <- constructQuery(sql.what, sql.where)

    # Convert full query to JSON for HTTP POST
    json.query <- toJSON(list(query = full.query, parameters = list()))

    # First set of brackets break the operation; remove them
    json.query <- str_replace(json.query, fixed("["), "")
    json.query <- str_replace(json.query, fixed("]"), "")

    # Generate auth header using specifications
    auth.header <- genHeader(verb = "POST", resource.type = res.type, resource.link = res.link, stored.time = ms.date.string, debug = debug.auth)

    # Assemble headers for the POST
    all.headers <- c("Authorization" = auth.header, "x-ms-version" = "2018-12-31", "x-ms-date" = ms.date.string, "Content-Type" = "application/query+json", "x-ms-documentdb-isquery" = "true", "x-ms-documentdb-query-enablecrosspartition" = "true", "x-ms-max-item-count" = max.items)
    # Check for continuation, if attempting to get pages
    if (continuation == TRUE) {
        if (!is.null(get0("continuation",envir = envCosmosDB))) {
            continuation.token <- c("x-ms-continuation" = get0("continuation",envir = envCosmosDB))
            all.headers <- append(all.headers, continuation.token)
            rm(continuation, envir = envCosmosDB)
        }
    }

    # Store the result of the call
    raw.response <- POST(post.uri, add_headers(.headers = all.headers), body = json.query)

    # Extract headers
    response.headers <- raw.response$headers

    # Extract continuation headers and token, store outside of function scope for next call
    continuation.header <- response.headers$`x-ms-continuation`
    assign("continuation", continuation.header, envir = envCosmosDB)

    # Send the status code of the POST to the console
    print(paste("Status Code is", raw.response$status_code, sep = " "))

    # Debug flag for viewing headers upon troubleshooting
    if (debug.query == TRUE) {
        print("*** Headers of Response ***")
        print(raw.response$headers)
        print(readBin(raw.response$content, "character"))
    }

    # Check content response flag; act accordingly
    if (content.response == FALSE) {
        raw.response
    } else if (content.response == TRUE) {
        char.response <- as.data.frame(readContent(raw.response)$Documents)
        if(content.flatten == TRUE)
            char.response = flatten(char.response)
        if(content.removecosmos == TRUE)
            char.response = subset(char.response, select = -c(`id`, `_rid`, `_self`, `_ts`, `_etag`, `_attachments`, `_ts`))

        char.response
    } else {
        print("Invalid content response option specified. Logical value required.")
    }
}

#' POST a full query to the REST API for Cosmos DB which returns all rows.
#'
#' @param sql.what String for specifying what fields to retrieve. Typically called select condition. Defaults to *
#' @param sql.where String for specifying what filter to use on data. Typically called search condition. Defaults to empty.
#' @return Returns the content
#' @keywords query cosmosdb post
#' @export
#' @examples
#' cosmosQueryAll(sql.what = "c.contact.eloquaId", sql.where = "c.contact.eloquaId != null")

cosmosQueryAll <- function(sql.what = "*", sql.where = "") {
    require(dplyr)
    allrecords <- data.frame()
    repeat{
        result <- cosmosQuery(sql.what = sql.what, sql.where = sql.where, continuation = TRUE, content.removecosmos = FALSE)
        allrecords <- bind_rows(allrecords, result)
        if (is.null(get0("continuation",envir = envCosmosDB)))
            break
    }
    allrecords
}
