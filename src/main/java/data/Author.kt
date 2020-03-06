package data

import com.fasterxml.jackson.annotation.JsonProperty

class Author (@JsonProperty("name") val name: String)
