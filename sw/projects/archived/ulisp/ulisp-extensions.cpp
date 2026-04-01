#include "ulisp-boxlambda.cpp"

// Table cross-reference functions

tbl_entry_t *tables[] = {lookup_table, lookup_table_boxlambda};
const int tablesizes[] = {
  arraysize(lookup_table),
  arraysize(lookup_table_boxlambda) };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}

