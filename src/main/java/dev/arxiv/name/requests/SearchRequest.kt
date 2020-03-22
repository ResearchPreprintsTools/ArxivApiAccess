package dev.arxiv.name.requests

import dev.arxiv.name.options.SearchField
import dev.arxiv.name.options.SearchOperator
import dev.arxiv.name.options.SortBy
import dev.arxiv.name.options.SortOrder

/**
 * The SearchRequestCreator represents methods for creating a search request to arxiv.org.
 * Based on arxiv.org documentation @see <a href="https://arxiv.org/help/api/user-manual">arxiv.org</a>
 *
 * If only search_query is given (id_list is blank or not given),
 * then the API will return results for each article that matches the search query.
 * If only id_list is given (search_query is blank or not given),
 * then the API will return results for each article in id_list.
 * If BOTH search_query and id_list are given,
 * then the API will return each article in id_list that matches search_query. This allows the API to act as a results filter.
 */
class SearchRequest(private val searchRequestBuilder: SearchRequestBuilder) {

    /**
     * Сreate's a string containing all specified query parameters
     *
     */
    fun createUrlAsString() = buildString {
        append("${searchRequestBuilder.arxivApiUri}?search_query=${createSearchQuery()}")
        searchRequestBuilder.sortBy?.let { append("&sortBy=${searchRequestBuilder.sortBy?.optionName}") }
        searchRequestBuilder.sortOrder?.let { append("&sortOrder=${searchRequestBuilder.sortOrder?.optionName}") }
        searchRequestBuilder.start?.let { append("&start=${searchRequestBuilder.start}") }
        searchRequestBuilder.maxResults?.let { append("&max_results=${searchRequestBuilder.maxResults}") }
        searchRequestBuilder.idList?.let { append("&id_list=${searchRequestBuilder.idList}") }
    }

    private fun createSearchQuery() : String = searchRequestBuilder.params.joinToString("") {
        "${it.searchOperator.optionName}${it.searchField.optionName}:${it.searchOption}"
    }

    class SearchRequestBuilder private constructor(searchOptions: String, searchField: SearchField) {
        var arxivApiUri: String = "http://export.arxiv.org/api/query"
        var sortBy: SortBy? = null
        var sortOrder: SortOrder? = null
        var start: Int? = null
        var maxResults: Int? = null
        var idList: String? = null
        var params: MutableList<SearchOptions> = mutableListOf()

        init {
            params.add(SearchOptions(searchOptions, searchField, SearchOperator.EMPTY))
        }

        companion object {
            fun create(searchOptions: String, searchField: SearchField) = SearchRequestBuilder(searchOptions, searchField)
        }

        fun and(searchOptions: String, searchField: SearchField) = apply {
            params.add(SearchOptions(searchOptions, searchField, SearchOperator.AND))
        }

        fun or(searchOptions: String, searchField: SearchField) = apply {
            params.add(SearchOptions(searchOptions, searchField, SearchOperator.OR))
        }

        fun andNot(searchOptions: String, searchField: SearchField) = apply {
            params.add(SearchOptions(searchOptions, searchField, SearchOperator.AND_NOT))
        }

        fun uri(value: String) = apply { arxivApiUri = value }

        fun sortBy(value: SortBy) = apply { sortBy = value }

        fun sortOrder(value: SortOrder) = apply { sortOrder = value }

        fun start(value: Int) = apply { start = value }

        fun maxResults(value: Int) = apply { maxResults = value }

        fun idList(value: String) = apply { idList = value }

        fun build(): SearchRequest {
            return SearchRequest(this)
        }
    }

    data class SearchOptions(val searchOption: String, val searchField: SearchField, val searchOperator: SearchOperator)
}
