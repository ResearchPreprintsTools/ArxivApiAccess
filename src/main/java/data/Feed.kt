package data

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper

data class Feed (@JsonProperty("link") val link: String,
                 @JsonProperty("title") val title: String,
                 @JsonProperty("id") val id: String,
                 @JsonProperty("updated") val updated: String,
                 @JsonProperty("totalResults") val totalResults: Int,
                 @JsonProperty("startIndex") val startIndex: Int,
                 @JsonProperty("itemsPerPage") val itemsPerPage: Int,
                 @JsonProperty("entry")
                 @JacksonXmlElementWrapper(useWrapping = false)
                 val entry: MutableList<Entry>?)
