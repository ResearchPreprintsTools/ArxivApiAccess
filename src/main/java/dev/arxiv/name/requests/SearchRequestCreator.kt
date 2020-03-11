package dev.arxiv.name.requests

import dev.arxiv.name.data.Feed
import dev.arxiv.name.options.SearchOperator
import dev.arxiv.name.options.SearchField
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
class SearchRequestCreator(mainSearchOptions: String, searchField: SearchField) {
    private var arxivApiUri: String = "http://export.arxiv.org/api/query"
    private var sortBy: SortBy? = null
    private var sortOrder: SortOrder? = null
    private var start: Int? = null
    private var maxResults: Int? = null
    private var idList: String? = null

    private var searchQuery: String = formSearchOption(mainSearchOptions, searchField)

    /**
     * Add's a new search dev.arxiv.name.options with the command "AND"
     *
     * @param searchOptions word to search
     * @param searchField name of fields to be searched
     * @return this
     */
    fun and(searchOptions: String, searchField: SearchField): SearchRequestCreator {
        searchQuery += SearchOperator.AND.optionName + formSearchOption(searchOptions, searchField)
        return this
    }

    /**
     * Add's a new search dev.arxiv.name.options with the command "OR"
     *
     * @param searchOptions word to search
     * @param searchField name of fields to be searched
     * @return this
     */
    fun or(searchOptions: String, searchField: SearchField): SearchRequestCreator {
        searchQuery += SearchOperator.OR.optionName + formSearchOption(searchOptions, searchField)
        return this
    }

    /**
     * Add's a new search dev.arxiv.name.options with the command "AND_NOT"
     *
     * @param searchOptions word to search
     * @param searchField name of fields to be searched
     * @return this
     */
    fun andNot(searchOptions: String, searchField: SearchField): SearchRequestCreator {
        searchQuery += SearchOperator.AND_NOT.optionName + formSearchOption(searchOptions, searchField)
        return this
    }

    /**
     * Sets a url of arvix API
     *
     * @param url a url of the arxiv API
     * @return this
     */
    fun url(url: String): SearchRequestCreator {
        this.arxivApiUri = url
        return this
    }

    /**
     * Sets a sortBy option
     *
     * @param sortBy Option for sorting
     * @return this
     */
    fun sortBy(sortBy: SortBy): SearchRequestCreator {
        this.sortBy = sortBy
        return this
    }

    /**
     * Sets a index of start element
     *
     * @param start index of start
     * @return this
     */
    fun start(start: Int): SearchRequestCreator {
        this.start = start
        return this
    }

    /**
     * Sets a numb of results a per page
     *
     * @param maxResult
     * @return this
     */
    fun maxResult(maxResult: Int): SearchRequestCreator {
        this.maxResults = maxResult
        return this
    }

    /**
     * Sets idList dev.arxiv.name.options
     *
     * @param idList a comma-delimited list of arXiv id's
     * @return this
     */
    fun idList(idList: String): SearchRequestCreator {
        this.idList = idList
        return this
    }

    /**
     * Sets a sortOrder option
     *
     * @param sortOrder type of sort order
     * @return this
     */
    fun sortOrder(sortOrder: SortOrder): SearchRequestCreator {
        this.sortOrder = sortOrder
        return this
    }

    /**
     * Сreate's a string containing all specified query parameters
     *
     */
    fun createUrlAsString() = buildString {
        append("${arxivApiUri}?search_query=$searchQuery")
        sortBy?.let { append("&sortBy=${sortBy?.optionName}") }
        sortOrder?.let { append("&sortOrder=${sortOrder?.optionName}") }
        start?.let { append("&start=$start") }
        maxResults?.let { append("&max_results=$maxResults") }
        idList?.let { append("&id_list=$idList") }
    }

    private fun formSearchOption(searchOptions: String, searchField: SearchField): String = "${searchField.optionName}:$searchOptions"
}
