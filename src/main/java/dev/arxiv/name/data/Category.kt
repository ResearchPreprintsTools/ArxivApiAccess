package dev.arxiv.name.data

import com.fasterxml.jackson.annotation.JsonProperty

data class Category (@JsonProperty("id") val term: String?)
