package dev.arxiv.name.utils

import dev.arxiv.name.data.Feed
import dev.arxiv.name.exeptions.LoadStepIsNotCorrect
import dev.arxiv.name.requests.SearchRequest
import dev.arxiv.name.requests.SearchRequestExecutor

/**
 * Loading all data by request and push it to callback.
 *
 * The best way is running that function in new thread.
 * also that function always ignores SearchRequestBuilder.start and SearchRequestBuilder.maxResults
 */
fun loadAllByRequest(requestBuilder: SearchRequest,
                     callback: (feed: Feed) -> Unit,
                     loadStep: Int = 100) {
    if (loadStep <= 0) {
        throw LoadStepIsNotCorrect("Load step should be more that 0. Actual value is $loadStep")
    }

    var startValue = 0

    val innerRequestBuilder: SearchRequest.SearchRequestBuilder = requestBuilder.cloneAsBuilder()
    val client = SearchRequestExecutor()

    do {
        innerRequestBuilder.start(startValue)
        innerRequestBuilder.maxResults(loadStep)

        val request = innerRequestBuilder.build()
        val response: Feed = client.executeAndMap(request)

        callback(response)

        startValue += loadStep

    } while (response.totalResults > (response.startIndex + loadStep))
}
