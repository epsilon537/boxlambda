# Namespace Management

## Word Lists

[wordlist.fs](../../../../fs/forth/boxkern-includes/wordlist.fs)
[compiler-memory.s](../../../../sw/components/forth/compiler-memory.s)

The wordlist module adds support for creating multiple wordlists. The module allows you to specify across which wordlists (and in which order) Word search (find) should take place and to which wordlist new Words should be added.

This module serves as a building block for the [Modules](#modules) module and should not be used directly by applications.

`forth`

- Constant. The wid of the default wordlist.

`set-order ( wid0 ... widn n -- )`

- Set the search order list and the length (n) of the search order list. `widn` is the first wordlist to search, `wid0` the last. May throw `x-order-overflow`.

`get-order ( -- wid0 ... widn n )`

- Retrieve the search order list and the length (n) of that list. `widn` is the first wordlist being searched, `wid0` is the last.

`wordlist ( -- wid )`

- Create a new wordlist and return its wordlist-id (`wid`).

`wordlist-name! ( c-addr wid -- )`

- Set the given counted string as wordlist name.

`wordlist-name@ ( wid -- c-addr )`

- Get the name of this wid as a counted string.

`.wordlist ( wid -- )`

- Print the wordlist info

`.order ( -- )`

- Print the wordlist search order, first-to-last.

`(current)`

- Variable. Points to the current wordlist. New words are added to the current wordlist.

`get-current ( -- wid )`

- Get the current wordlist. New words are added to the current wordlist.

`set-current ( wid -- )`

- Set the current wordlist. New words are added to the current wordlist.

`wordlist-list ( wid -- )`

- List all the words in a given wordlist.

`dictionarynext ( link-addr -- addr flag)`

- Return next entry in dictionary chain (search-order aware). Returns a Word link address and false if a Word is found. Returns 0 true if end is reached.

`dictionarystart ( -- link-addr )`

- This is the wordlist search-order aware version of `dictionarystart`.
 Returns the link field of the first Word of the first wordlist in the search-order.

`dictionarystart-all-wids ( -- link-addr wid )`

- This version of `dictionarystart` scans across all wids, regardless of the configured search-order. Both the Word's link address and the wid it belongs to are returned.

`dictionarynext-all-wids ( link-addr -- addr wid|0 )`

- Scans dictionary chain across all wids, returns a Word's link address and its wid if a Word is found. Returns 0 0 if end is reached.

`(find-wordlist) ( addr len -- code-address flags )`

- This is the wordlist search-order aware version of find. It works like the forth core's find Word, but it's using the redefined search-order aware `dictionarystart/next` Words.

`wordlist-init ( -- )`

- Initializes the wordlist module. Executes during `wordlist.fs` evaluation.

### Exceptions

`x-wid-overflow ( -- )`

- Max. number of wids exceeded.

`x-order-overflow ( -- )` 

- Search-Order list overflow.

`x-empty-search-order ( -- )` 

- Search-Order list empty.

## Modules

[module.fs](../../../../fs/forth/boxkern-includes/module.fs)

Adds support for creating and managing modules (namespaces). Builds on the [wordlist module](#word-lists).

`::' ( wid "name" -- xt )`

- Get the xt of the following Word, to be found in given namespace. E.g. `mymod ::' foo`.

`::['] ( wid "name" -- xt )`

- Compile into the definition the xt of the following Word, to be found in given namespace. E.g. `mymod ::['] foo`.

`begin-module ( "name" -- )`

- Create a new module/namespace with the given name. This creates a new wordlist, makes it current, and puts it on top of the search order. Assigns the wordlist id (`wid`) to a constant with the passed-in name. This constant becomes the module identifier.

`continue-module ( wid -- )`

- Extend the given module/namespace. It works like begin-module but takes an existing module `wid` as input parameter rather than creating a new one.

`end-module ( -- )`

- Revert search-order and current to the state before `begin-module`.

`import ( module -- )`

- Add the given module to the top of the wordlist search order.

`unimport ( module -- )`

- Remove the given module from the search-order. If the module appears more than once in the search-order, only the top-most entry is removed.

### Example Usage

```
begin-module module-a
  : foo ." module-a foo" cr ;
end-module

begin-module module-b
  : foo ." module-b foo" cr ;
end-module

module-a :: foo \ executes module-a's foo.
module-b :: foo \ executes module-b's foo.

module-a import
foo \ executes module-a's foo.
module-a unimport

module-b import
foo \ executes module-b's foo.
module-b unimport
```

