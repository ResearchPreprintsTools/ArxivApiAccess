package requests

import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.dataformat.xml.XmlMapper
import data.Feed
import io.ktor.client.HttpClient
import io.ktor.client.engine.apache.Apache
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking

fun executeSearchRequest(url: String): Feed {
    val mapper = XmlMapper().configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true)

    return mapper.readValue(executeRequest(url), Feed::class.java)
}

private fun executeRequest(url: String): String = runBlocking {
    HttpClient(Apache).get<String>(url)
}

