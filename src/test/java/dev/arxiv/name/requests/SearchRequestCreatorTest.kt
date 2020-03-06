package dev.arxiv.name.requests

import dev.arxiv.name.options.SearchOperator
import dev.arxiv.name.options.SearchField
import dev.arxiv.name.options.SortBy
import dev.arxiv.name.options.SortOrder
import org.junit.jupiter.api.Test

import org.junit.jupiter.api.Assertions.*

internal class SearchRequestCreatorTest {
    private val mainRequestName = "test"

    private val baseUri = "http://export.arxiv.org/api/query"

    @Test
    fun `Creating with the prefix's`() {
        SearchField.values().forEach { prefix ->
            val url: String = SearchRequestCreator(mainRequestName, prefix).createUrlAsString()
            assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName", url)
        }
    }

    @Test
    fun `Creating with the "AND" option`() {
        val andOptions = "java"
        val prefix = SearchField.ALL
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .and(andOptions, prefix)
                .createUrlAsString()

        assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName${SearchOperator.AND.optionName}${prefix.optionName}:$andOptions", url)
    }

    @Test
    fun `Creating with the "OR" option`() {
        val orOptions = "java"
        val prefix = SearchField.ALL
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .or(orOptions, prefix)
                .createUrlAsString()

        assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName${SearchOperator.OR.optionName}${prefix.optionName}:$orOptions", url)
    }

    @Test
    fun `Creating with the "AND NOT" option`() {
        val andNotOptions = "java"
        val prefix = SearchField.ALL
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .andNot(andNotOptions, prefix)
                .createUrlAsString()

        assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName${SearchOperator.AND_NOT.optionName}${prefix.optionName}:$andNotOptions", url)
    }

    @Test
    fun `Creating with a user uri`() {
        val prefix = SearchField.ALL
        val testUrl = "http://mytest.url/somerequest"
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .url(testUrl)
                .createUrlAsString()

        assertEquals("$testUrl?search_query=${prefix.optionName}:$mainRequestName", url)
    }

    @Test
    fun `Creating with the "SortBy" options`() {
        val prefix = SearchField.ALL
        SortBy.values().forEach { sortBy ->
            val url: String = SearchRequestCreator(mainRequestName, prefix)
                    .sortBy(sortBy)
                    .createUrlAsString()

            assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName&sortBy=${sortBy.optionName}", url)
        }
    }

    @Test
    fun `Creating with the "Start" options`() {
        val start = 1
        val prefix = SearchField.ALL
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .start(start)
                .createUrlAsString()

        assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName&start=${start}", url)
    }

    @Test
    fun `Creating with the "maxResults" options`() {
        val maxResult = 1
        val prefix = SearchField.ALL
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .maxResult(maxResult)
                .createUrlAsString()

        assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName&max_results=${maxResult}", url)
    }

    @Test
    fun `Creating with the "idList" options`() {
        val idList = "1,2,3"
        val prefix = SearchField.ALL
        val url: String = SearchRequestCreator(mainRequestName, prefix)
                .idList(idList)
                .createUrlAsString()

        assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName&id_list=${idList}", url)
    }

    @Test
    fun `Creating with the "SortOrder" options`() {
        val prefix = SearchField.ALL
        SortOrder.values().forEach { sortOrder ->
            val url: String = SearchRequestCreator(mainRequestName, prefix)
                    .sortOrder(sortOrder)
                    .createUrlAsString()

            assertEquals("$baseUri?search_query=${prefix.optionName}:$mainRequestName&sortOrder=${sortOrder.optionName}", url)
        }
    }
}
