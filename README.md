# aebytecode
An library and stand alone assembler for aeternity bytecode.

This version supports Aevm bytecode and Fate bytecode.

## Build

    $ make

## Fate Code

Fate code exists in 3 formats:

1. Fate byte code. This format is under consensus.
2. Fate assembler. This is a text represenation of fate code.
                     This is not under consensus and other
                     implemenation and toolchains could have
                     their own format.
3. Internal. This is an Erlang representation of fate code
               Used by this particular engin implementation.

This library handles all tree representations.
The byte code format is described in a separate document.
The internal format is described in a separate document.
The text representation is described below.

### Fate Assembler Code

Assembler code can be read from a file.
The assembler has the following format:

Comments start with 2 semicolons and runs till end of line
`;; This is a comment`

Opcode mnemonics start with an upper case letter.
`DUP`

Identifiers start with a lower case letter
`an_identifier`

References to function arguments start with arg followed by an integer
`arg0`

References to variables/registers start with var followed by an integer
`var0`

References to stack postions is either a (for stack 0)
or start with stack followed by an integer
`stack1`
`a`

Immediate values can be of 9 types:

1. Integers as decimals: {Digits} or -{Digits}
 `42`
 `-2374683271468723648732648736498712634876147`
  And integers as Hexadecimals::  0x{Hexdigits}
 `0x0deadbeef0`

2. addresses, a base58 encoded string starting with #{base58char}
 followed by up to 64 hex chars
 `#nv5B93FPzRHrGNmMdTDfGdd5xGZvep3MVSpJqzcQmMp59bBCv`

3. Boolean  true or false
 `true`
 `false`

4. Strings  "{Characters}"
 `"Hello"`

5. Map  { Key => Value }
 `{}`
 `{ 1 => { "foo" => true, "bar" => false}`

6. Lists [ Elements ]
 `[]`
 `[1, 2]`

7. Bit field < Bits > or !< Bits >
 `<000>`
 `<1010 1010>`
 `<>`
 `!<>`

8. Tuples ( Elements )
 `()`
 `(1, "foo")`

9. Varaiants: (| Size | Tag | ( Elements ) |)
 `(| 42 | 12 | ( "foo", 12) |)`

Where

Digits: [0123456789]

Hexdigits:  [0123456789abcdef]

base58char:  [123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz]

Characters: any printable ascii character 0..255 (except " no quoting yet)

Key: any value except for a map

Bits: 01 or space

Elements: Nothing or Value , Elements

Size: Digits (0 < Size < 256)

Tag: Digits (0 =< Tag < Size)

