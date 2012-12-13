Ketama
======

Ketama is a pure Erlang implementation of [libketama], a consistent hashing
library created by the nice folks at [Last.fm]. It is notably used by PHP's
memcached extension and similar projects in other programming languages.

Implementation notes
--------------------

A continuum is stored in an ETS table of type `ordered_set`. When the continuum
is updated (when adding new weights or removing values), the table is updated
by first adding the new points and then deleting the ones which are now
obsolete.

License
-------

Under the terms of the [ISC license].


[libketama]:   https://github.com/RJ/ketama
[Last.fm]:     http://www.last.fm
[ISC license]: http://en.wikipedia.org/wiki/ISC_license
