# Dictionary Tools

[pre-dict.fs](../../../fs/forth/boxkern-includes/pre-dict.fs)
[dict.fs](../../../fs/forth/boxkern-includes/dict.fs)

## Flag Field Constants

```
-1 constant FLAG-INVISIBLE
$10 constant FLAG-IMMEDIATE
$80 constant FLAG-RAMALLOT
$100 constant FLAG-BUFFER
```

## Words

` link>link ( addr -- addr* )`

- Get the address of the link field of a dictionary entry.

`link>flags ( addr -- addr* )`

- Get the address of the flags field of a dictionary entry.

`link>name  ( addr -- addr* )`

- Get the address of the name field of a dictionary entry.

`link>code  ( addr -- addr* )`

- Get the address of the code field of a dictionary entry.

`inside-code>link ( addr-inside -- link|0 )`

- Try to find the given address inside of a definition.

`link>wid  ( link -- wid | 0 )`

- Find the wordlist a given link belongs to. 

`code>link ( entrypoint -- addr | 0 )`

- Try to find this code start address in dictionary. Wordlist aware.

`variable>link ( location -- addr | 0 )`

- Try to find this variable or buffer in dictionary. Wordlist aware.

`variable-name. ( addr -- )`

- Print the name of this variable or buffer.

`forget ( "name" -- )`

- Forget name.

`del ( -- )` 

- Remove the latest definition from the dictionary.

`traceinside. ( addr -- )`

- Find which Word addr belongs to.

