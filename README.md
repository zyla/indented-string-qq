# indented-string-qq

A quasiquoter that helps insert text content as string literals, without caring
about indentation.

Example:

```haskell
myStringLiteral =
  [indented|
    foo
      bar
    baz
  |]
```

will produce the string `"foo\n  bar\nbaz"`.
