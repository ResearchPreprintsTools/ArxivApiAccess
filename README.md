[![Build Status](https://travis-ci.com/olegthelilfix/ArxivApiAccess.svg?branch=master)](https://travis-ci.com/olegthelilfix/ArxivApiAccess)
[![GuardRails badge](https://badges.guardrails.io/olegthelilfix/ArxivApiAccess.svg?token=b7c2657f559528c6c5b76c14c0a07f739b50503091369b47dd5cab61e41cbe8b&provider=github)](https://dashboard.guardrails.io/default/gh/olegthelilfix/ArxivApiAccess)
[![GitHub License](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg?style=flat)](http://www.apache.org/licenses/LICENSE-2.0)
# ArvixApiAccess 
ArvixApiAccess is Kotlin written library to make search requests to API of Arvix.org

## Getting started with Maven
``` xml
<dependency>
  <groupId>olegthelilfix</groupId>
  <artifactId>ArxivApiAccess</artifactId>
  <version>${ArxivApiAccess.version}</version>
</dependency>
```

## A simple example of usage
``` kotlin
val request: SearchRequestCreator = SearchRequestCreator("Java", SearchField.ALL)
            .or("Kotlin", SearchField.ALL)
            .andNot("Groovy", SearchField.ALL)
            .sortBy(SortBy.LAST_UPTATED_DATE)
            .sortOrder(SortOrder.ASCENDING)
            .maxResult(20)

val response: Feed = executeSearchRequest(request)
    
println(response)
```

