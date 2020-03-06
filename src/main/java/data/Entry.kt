package data

import com.fasterxml.jackson.annotation.JsonProperty
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper

data class Entry (@JsonProperty("id") val id: String,
                  @JsonProperty("updated") val updated: String,
                  @JsonProperty("published") val published: String,
                  @JsonProperty("title") val title: String,
                  @JsonProperty("summary") val summary: String,
                  @JsonProperty("author")
                  @JacksonXmlElementWrapper(useWrapping = false)
                  val author: MutableList<Author>,
                  @JsonProperty("link")
                  @JacksonXmlElementWrapper(useWrapping = false)
                  val link: MutableList<Link>)
