package dev.arxiv.name.data

import com.fasterxml.jackson.annotation.JsonProperty

data class Link(
        @JsonProperty("title") val title: String?,
        @JsonProperty("href") val href: String?,
        @JsonProperty("rel") val rel: String?,
        @JsonProperty("type") val type: String?
)
