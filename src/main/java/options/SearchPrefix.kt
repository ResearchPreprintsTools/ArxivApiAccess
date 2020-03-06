package options

enum class SearchPrefix(val optionName: String) {
    TITLE("ti"),
    AUTHOR("au"),
    ABSTRACT("abs"),
    COMMENT("co"),
    JOURNAL_REFERENCE("jr"),
    SUBJECT_CATEGORY("cat"),
    REPORT_NUMBER("rn"),
    ALL("all")
}
