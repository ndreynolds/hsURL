hsURL
=====

hsURL is a Haskell library and command line tool for making HTTP requests,
modeled after cURL. I've been writing this while learning Haskell (so pull
requests with style or code improvements are appreciated :).

The project implements the functionality necessary to make requests over the
Hypertext Transfer Protocol (HTTP/1.1) given a URL and any additional request
options. This includes:

* Parsing URIs to extract the various components (e.g., host, port, path)
* Building and formatting the request
* Opening a socket and sending the formatted request
* Listening for and parsing the response

The TCP/IP layers are handled by the `Network` module and the parsers are
implemented using the Parsec library.

**This project is a work-in-progress.** The basics are functional, but I'm
currently working on adding additional options and tests, as well as improving
spec compliance.


Installing
----------

hsURL can be built with cabal. You probably want to create a sandbox first, e.g.

```bash
$ cabal sandbox init
$ cabal install
```

The generated executable is standalone and can be moved to where ever you like, e.g.

```bash
$ cp dist/build/hsurl/hsurl ~/bin/
```

Usage
-----

### Making a simple GET request

**From GHCI:**

```haskell
Prelude> let request = Http.Request.get "http://www.google.com"
Prelude> :t request
request :: Http.Request.Request
Prelude> let response = Http.Request.makeRequest request
Prelude> :t response
response :: IO Http.Response.Response
```

The response parts can then be examined individually 
(need to use `liftM` or similar to deal with the IO monad):

```haskell
Prelude> liftM Http.Response.statusLine response
StatusLine {code = 200, reasonPhrase = "OK", version = "HTTP/1.1"}
Prelude> liftM Http.Response.headers response
[("Date","Wed, 18 Feb 2015 20:54:22 GMT"),("Expires","-1"),("Cache-Control","private, max-age=0"),...]
Prelude> liftM Http.Response.body response
"<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org/WebPage\" lang=\"en\"><head><meta..."
```

**Using the command-line client:**

```bash
$ hsurl http://www.google.com
<!doctype html><html itemscope=\"\" itemtype=\"http://schema.org/WebPage\" lang=\"en\"><head><meta...
```

### Making a POST request

TODO

### Setting the HTTP method

TODO

### Setting headers

TODO

### Other options

TODO


Future Additions
----------------

Time permitting, I'd like to add the following features:

* Streaming uploads 
* HTTPS/TLS support via (hs-tls)[https://github.com/vincenthz/hs-tls]

