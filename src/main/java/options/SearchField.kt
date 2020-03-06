package options

/**
 * The SearchPrefix sets the names of the fields to be searched.
 */
enum class SearchField(val optionName: String) {
    TITLE("ti"),
    AUTHOR("au"),
    ABSTRACT("abs"),
    COMMENT("co"),
    JOURNAL_REFERENCE("jr"),
    SUBJECT_CATEGORY("cat"),
    REPORT_NUMBER("rn"),
    ALL("all")
}
