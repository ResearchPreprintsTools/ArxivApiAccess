<a href="https://www.buymeacoffee.com/8IPzWFK" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/default-black.png" alt="Buy Me A Coffee" width="217" height="51"></a>

[![Build Status](https://travis-ci.com/olegthelilfix/ArxivApiAccess.svg?branch=master)](https://travis-ci.com/olegthelilfix/ArxivApiAccess)
[![GuardRails badge](https://badges.guardrails.io/olegthelilfix/ArxivApiAccess.svg?token=b7c2657f559528c6c5b76c14c0a07f739b50503091369b47dd5cab61e41cbe8b&provider=github)](https://dashboard.guardrails.io/default/gh/olegthelilfix/ArxivApiAccess)
[![GitHub License](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg?style=flat)](http://www.apache.org/licenses/LICENSE-2.0)
# ArvixApiAccess 
ArvixApiAccess is Kotlin written open-source library to make search requests to API of Arvix.org

## Getting started with Maven
In order to start using you should add that dependency to your project.
``` xml
<repository>
    <id>myMavenRepoRead</id>
    <url>https://mymavenrepo.com/repo/m3NxrnIEacYdXdF77zDL/</url>
</repository>

...

<dependency>
  <groupId>olegthelilfix</groupId>
  <artifactId>ArxivApiAccess</artifactId>
  <version>${ArxivApiAccess.version}</version>
</dependency>
```

## A simple example of usage
``` kotlin
// Request to find the last 20 articles about Java or Kotlin 
val request: SearchRequest = SearchRequest.SearchRequestBuilder
            .create("Java", SearchField.ALL)
            .or("Kotlin", SearchField.ALL)
            .sortBy(SortBy.LAST_UPTATED_DATE)
            .sortOrder(SortOrder.DESCENDING)
            .maxResults(20)
            .build()

val response: Feed = SearchRequestExecutor().executeAndMap(request)

val responseAsString: String = SearchRequestExecutor().execute(request)
    
println(response)
println(responseAsString)
```

## load all by request to file
``` kotlin
val subjectCategory = "JAVA"
val fileToSave = "../arxiv_data_sets/${subjectCategory}.csv"

// All article about java
val request = SearchRequest.SearchRequestBuilder
        .create(subjectCategory, SearchField.ALL)
        .build()

loadAllByRequest(request, {feed ->
    // save to file 
    val writer = FileWriter(fileToSave, true)
    writer.use {
        feed.entry?.forEach {
            val authorString = it.author.joinToString(", ") { author -> author.name }
            val resultString = "${it.updated}|${subjectCategory}|${it.title}|${authorString}".replace("\n", "")
            writer.write("${resultString}\n")
        }
        writer.close()
    }

    println("parsed ${feed.startIndex+feed.itemsPerPage}/${feed.totalResults}")
    // return false to stop a process of the parsing
    true
})
``` 

## load all after date
``` kotlin
// the request to find all articles about Java or Kotlin for last 30 days
val request: SearchRequest = SearchRequest.SearchRequestBuilder
        .create("Java", SearchField.ALL)
        .or("Kotlin", SearchField.ALL)
        .build()

var list = searchAllAfterDate(request, Date(System.currentTimeMillis() - TimeUnit.MILLISECONDS.convert(30, TimeUnit.DAYS)))

println(list)
``` 

## mapping of subjects
``` kotlin
feed.entry?.forEach {
    it.category.forEach { category ->
        try {
            val categoryFullName = convertTermCode(category.term)
            println("Mapped ${category.term} to ${categoryFullName}")
        }
        catch (e: TermNotFoundException) {
            println(e.message)
        }
    }
}
``` 

#linked projects
@articleflowbot or [ArticleFlow github](https://github.com/olegthelilfix/ArticleFlow) is the telegram bot to make search request to arxiv.org

## contacts
Feel free to email me at [olegthelilfix@pm.me](mailto:olegthelilfix@pm.me)

