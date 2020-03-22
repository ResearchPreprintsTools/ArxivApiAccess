package dev.arxiv.name.requests

import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.dataformat.xml.XmlMapper
import dev.arxiv.name.data.Feed
import io.ktor.client.HttpClient
import io.ktor.client.engine.apache.Apache
import io.ktor.client.request.get
import kotlinx.coroutines.runBlocking

/**
 * That class execute search a request and map a result of response to Object
 */
class SearchRequestExecutor(private val client: HttpClient = HttpClient(Apache)) {
    private val mapper = XmlMapper()
            .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
            .configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true)

    /**
     * Execute request and return result as Feed object
     */
    fun executeAndMap(request: SearchRequest): Feed {
        return mapper.readValue(execute(request), Feed::class.java)
    }

    /**
     * Execute request and return result as String
     */
    fun execute(request: SearchRequest): String {
        return runBlocking {
            client.get<String>(request.createUrlAsString())
        }
    }
}
