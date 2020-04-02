package dev.arxiv.name.utils

import dev.arxiv.name.data.Entry
import dev.arxiv.name.data.Feed
import dev.arxiv.name.exeptions.LoadStepIsNotCorrect
import dev.arxiv.name.options.SortBy
import dev.arxiv.name.options.SortOrder
import dev.arxiv.name.requests.SearchRequest
import dev.arxiv.name.requests.SearchRequestExecutor
import java.util.*

/**
 * Loading all data by request and push it to callback.
 *
 * The best way is running that function in new thread.
 * also that function always ignores SearchRequestBuilder.start and SearchRequestBuilder.maxResults
 */
fun loadAllByRequest(request: SearchRequest, callback: (feed: Feed) -> Boolean, loadStep: Int = 100) {
    if (loadStep <= 0) {
        throw LoadStepIsNotCorrect("Load step should be more that 0. Actual value is $loadStep")
    }

    var startValue = 0

    val innerRequestBuilder: SearchRequest.SearchRequestBuilder = request.cloneAsBuilder()
    val client = SearchRequestExecutor()

    do {
        innerRequestBuilder.start(startValue)
        innerRequestBuilder.maxResults(loadStep)

        val response: Feed = client.executeAndMap(innerRequestBuilder.build())

        startValue += loadStep

    } while (callback(response) && response.totalResults > (response.startIndex + loadStep))
}


/**
 * Loading all article after date
 * Function ignore SearchRequestBuilder.start, SearchRequestBuilder.maxResults, SearchRequestBuilder.sortBy and
 * SearchRequestBuilder.sortOrder
 */
fun searchAllAfterDate(request: SearchRequest, date: Date, loadStep: Int = 100): List<Entry> {
    val result = mutableListOf<Entry>()

    val innerRequest = request.cloneAsBuilder()
            .sortBy(SortBy.LAST_UPTATED_DATE)
            .sortOrder(SortOrder.DESCENDING)
            .build()

    loadAllByRequest(request = innerRequest, loadStep = loadStep, callback = { feed ->
        feed.entry?.let { entry ->
            result.addAll(entry.filter { article ->
                article.published >= date
            })
        }

        val lastArticlePublishedDate: Date? = feed.entry?.last()?.published

        if (lastArticlePublishedDate != null) lastArticlePublishedDate >= date else false
    })

    return result
}
