package requests

import options.SearchOperator
import options.SearchPrefix
import options.SortBy
import options.SortOrder

class SearchRequestBuilder(mainSearchOptions: String, searchPrefix: SearchPrefix) {
    private var arxivApiUri: String = "http://export.arxiv.org/api/query"
    private var sortBy: SortBy? = null
    private var sortOrder: SortOrder? = null
    private var start: Int? = null
    private var maxResults: Int? = null
    private var idList: String? = null

    private var searchQuery: String = formSearchOption(mainSearchOptions, searchPrefix)

    fun and(searchOptions: String, searchPrefix: SearchPrefix): SearchRequestBuilder {
        searchQuery += SearchOperator.AND.optionName + formSearchOption(searchOptions, searchPrefix)
        return this
    }

    fun or(searchOptions: String, searchPrefix: SearchPrefix): SearchRequestBuilder {
        searchQuery += SearchOperator.OR.optionName + formSearchOption(searchOptions, searchPrefix)
        return this
    }

    fun andNot(searchOptions: String, searchPrefix: SearchPrefix): SearchRequestBuilder {
        searchQuery += SearchOperator.AND_NOT.optionName + formSearchOption(searchOptions, searchPrefix)
        return this
    }

    fun url(url: String): SearchRequestBuilder {
        this.arxivApiUri = url
        return this
    }

    fun setSortBy(sortBy: SortBy): SearchRequestBuilder {
        this.sortBy = sortBy
        return this
    }

    fun setStart(start: Int): SearchRequestBuilder {
        this.start = start
        return this
    }

    fun setMaxResult(maxResult: Int): SearchRequestBuilder {
        this.maxResults = maxResult
        return this
    }

    fun setIdList(idList: String): SearchRequestBuilder {
        this.idList = idList
        return this
    }

    fun setSortOrder(sortOrder: SortOrder): SearchRequestBuilder {
        this.sortOrder = sortOrder
        return this
    }

    fun buildRequestAsString() = buildString {
        append("${arxivApiUri}?search_query=$searchQuery")
        sortBy?.let { append("sortBy=$sortBy") }
        sortOrder?.let { append("sortOrder=$sortBy") }
        start?.let { append("start=$start") }
        maxResults?.let { append("max_results=$maxResults") }
        idList?.let { append("id_list=$idList") }
    }

    private fun formSearchOption(searchOptions: String, searchPrefix: SearchPrefix): String = "${searchPrefix.optionName}:$searchOptions"
}
