out.table_notes<-function(table,warns) {
  for (w in warns)
    table$setNote(w,w)
}