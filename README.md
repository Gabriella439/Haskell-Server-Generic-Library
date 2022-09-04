# `server-generic` v1.0.0

Use this library to auto-generate an API service that parses a typed value.
This library uses Haskell's support for generic programming to customize the
API to the type of value that you request.

For example, the following program:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Server.Generic
import System.Directory (removeFile)

data Command
    = Create { filepath :: FilePath, contents :: String }
    | Delete { filepath :: FilePath }
    deriving (Generic)

instance ParseRecord Command

handler :: Command -> IO String
handler (Create file text) = do
    writeFile file text
    return "File created"
handler (Delete file) = do
    removeFile file
    return "File deleted"

main :: IO ()
main = serveJSON 8080 handler
```

... generates two API endpoints which `/create` and `/delete` files:

```bash
$ curl 'localhost:8080/create?filepath=test.txt&contents=ABC'
"File created"
$ cat test.txt
ABC
$ curl 'localhost:8080/delete?filepath=test.txt'
"File deleted"
$ cat test.txt
cat: test.txt: No such file or directory
```

If we don't provide field labels

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Server.Generic

data Command
    = Add Double Double
    | Multiply Double Double
    deriving (Generic)

instance ParseRecord Command

handler :: Command -> IO Double
handler (Add      x y) = return (x + y)
handler (Multiply x y) = return (x * y)

main :: IO ()
main = serveJSON 8080 handler
```

... then the arguments become path tokens instead of query parameters:

```bash
$ curl 'localhost:8080/add/2/3'
5
$ curl 'localhost:8080/multiply/2/3'
6
```

This library tries to be as intelligent as possible:

* Labeled fields are read from query parameters
* Unlabeled fields are read from path tokens
* Every constructor generates an API endpoint of the same name (lowercased)
* The `Maybe` type constructor translates to an optional token/parameter
* The `[]` type constructor translates to a repeated token/parameter
* `Any`/`All`/`First`/`Last`/`Sum`/`Product` also translate to repeated
  tokens/parameters, but with different behaviors (i.e. `First` will return the
  first token/parameter that matches and `Sum` will sum them all)

For the full tutorial, read the
[Hackage documentation](http://hackage.haskell.org/package/server-generic/docs/Server-Generic.html).

## Development status

[![Build Status](https://travis-ci.org/Gabriella439/Haskell-Server-Generic-Library.png)](https://travis-ci.org/Gabriella439/Haskell-Server-Generic-Library)

This library will still be in some flux since I'm not really a web developer and
I probably got some design decisions wrong.  Most changes will be either:

* Improving idioms to better match what web developers expect
* Improving error messages when the client supplies an invalid route
* Finding out a way to specify how to handle the request method in the type

## LICENSE (BSD 3-Clause)

Copyright (c) 2016 Gabriella Gonzalez  
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright notice,
  this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
* Neither the name of Gabriella Gonzalez nor the names of other contributors
  may be used to endorse or promote products derived from this software
  without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
