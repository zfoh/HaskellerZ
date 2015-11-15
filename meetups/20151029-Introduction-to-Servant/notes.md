<style>
  body { font-family: sans-serif };
</style>


About Me
--------

* Martin Geisler, <http://geisler.net/>

* Software developer at Rackspace Zurich

* Full-stack development in Python and JavaScript

* Have dabbled with Haskell for a long time, but only for fun in my
  free time


Swift
-----

* OpenStack Swift is an eventually consistent scalable object store

* Millions of containers with millions of objects

* Redundant storage for reliability

* Simple RESTful API for retrieving, updating, and deleting objects:
  <http://developer.openstack.org/api-ref-objectstorage-v1.html>


Web Application Interface
-------------------------

* WAI is a web application interface (like WSGI for Python):
  <https://hackage.haskell.org/package/wai/docs/Network-Wai.html>

* Warp is a HTTP server for running WAI applications:
  <http://aosabook.org/en/posa/warp.html#warps-architecture>


Servant
-------

* Framework for building types RESTful web services:
  <http://haskell-servant.github.io/>

* Servant has the standard terse API documentation I see everywhere in
  Haskell. However, it makes up for this with a great tutorial that
  has many practical examples:
  <http://haskell-servant.github.io/tutorial/>


Installation
------------

To get Servant together with Warp (a WAI webserver) simply run:

    $ cabal install servant-server


Out of the Box
--------------

Servant will handle simple things out of the box:

* Returns `404 Not Found` for undefined routes

* Returns `405 Method Not Allowed` when an endpoint exists, but
  doesn't respond to the HTTP method used

* Returns `204 Accepted` when a route doesn't return anything

* Handles `HEAD` requests, but see below


Generating Documentation and JavaScript Clients
-----------------------------------------------

API endpoints are fully described with the Haskell type system. This
allows Servant to auto-generate API documentation and JavaScript
clients. Several JavaScript clients can be generated:

* Vanilla XMLHttpRequest clients

* jQuery clients

* AngularJS clients


Examples
--------

1. Hello World

2. Multiple endpoints

3. JSON output

4. Accept header

5. Response headers

6. Stateful server

7. POST requests


HEAD Requests
-------------

* Need unreleased version 0.5 (commit 050aa21b9d1f) to respond to
  `HEAD` requests

* Easy to install into a Cabal sandbox from a local Git clone:

        $ cabal sandbox init
        $ git clone https://github.com/haskell-servant/servant.git ../servant
        $ cabal sandbox add-source ../servant/servant
        $ cabal sandbox add-source ../servant/servant-server
        $ cabal update
        $ cabal install servant-server


PlainText Responses
-------------------

* Need unreleased version 0.5 (commit 153de01a6290) for `MimeRender
  PlainText String` instance


Problems
--------

Servant works really well for the use-case it was meant to solve:
uniform APIs where all endpoints plus their incoming and outgoing
headers are known upfront. Handling more "loose" API seem difficult:

* Routes that accept more than one path segment from `PATH_INFO`
  doesn't seem to be supported -- you will apparently need to use Raw

* Accepting arbitrary content types seem to be unsupported -- again
  one probably needs to use Raw

* Returning arbitrary headers seem difficult -- Raw to the rescue
  again.
