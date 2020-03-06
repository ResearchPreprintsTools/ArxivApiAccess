package data

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper

/**
 * The Feed contains the request metadata
 */
data class Feed(
        /**
         * Provides a URL that can be used to retrieve this feed again
         */
        @JsonProperty("link") val link: String,
        /**
         * Contains a canonicalized version of a used query
         */
        @JsonProperty("title") val title: String,
        /**
         * Contains a unique id for a query
         */
        @JsonProperty("id") val id: String,
        /**
         * Contains the last time the contents of the feed were last updated
         */
        @JsonProperty("updated") val updated: String,
        /**
         * Contains the count of available results by request
         */
        @JsonProperty("totalResults") val totalResults: Int,
        /**
         * Contains the index of the first returned result
         */
        @JsonProperty("startIndex") val startIndex: Int,
        /**
         * Contains the counts of search results per page
         */
        @JsonProperty("itemsPerPage") val itemsPerPage: Int,
        /**
         * Contains articles information.
         * If content by request had not found, that field will be NULL.
         */
        @JsonProperty("entry")
        @JacksonXmlElementWrapper(useWrapping = false)
        val entry: List<Entry>?)
