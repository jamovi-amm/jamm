# Shared SmartTabs initialization, matching the GAMLj lifecycle helper.
init_smart_tables <- function(objects, mute = FALSE) {
    for (tab in objects) {
        tab$initTable()
        tab$mutenotes <- mute
    }
    objects
}
