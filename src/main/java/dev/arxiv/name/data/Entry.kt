package dev.arxiv.name.data

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper
import java.util.*

/**
 * The Entry contains information about an article
 */
data class Entry(
        /**
         * Contains a url that resolves to the abstract page for that article
         */
        @JsonProperty("id") val id: String,
        /**
         * Contains the date on which the retrieved article was submitted and processed
         */
        @JsonProperty("updated") val updated: Date,
        /**
         * Contains the date in which the first version of this article was submitted and processed
         */
        @JsonProperty("published") val published: Date,
        /**
         * Contains the title of the article
         */
        @JsonProperty("title") val title: String,
        /**
         * Contains the abstract for the article
         */
        @JsonProperty("summary") val summary: String,
        /**
         * Contains for each author of the paper in order of authorship
         */
        @JsonProperty("author")
        @JacksonXmlElementWrapper(useWrapping = false)
        val author: List<Author>,
        /**
         * Contains a term, that is used to describe either an arXiv, ACM, or MSC classification
         */
        @JsonProperty("category")
        @JacksonXmlElementWrapper(useWrapping = false)
        val category: List<Category>,
        /**
         * Contains links on a article text
         */
        @JsonProperty("link")
        @JacksonXmlElementWrapper(useWrapping = false)
        val link: List<Link>
)
