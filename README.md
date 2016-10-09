Logoot
---

Simple Logoot implementation.

Logoot is a [Conflict-free Replicated Data Type][crdt] (CRDT) created to be used by
distributed systems that want to achieve Strong Eventual Consistency (SEC).

[crdt]: https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type

This is an implementation of [Logoot-undo][logoot] propose by Stéphane Weiss,
Pascal Urso and Pascal Molli. It still lacks support for undo operations.

[logoot]: https://pdfs.semanticscholar.org/75e4/5cd9cae6d0da1faeae11732e39a4c1c7a17b.pdf

There are a lot of missing pieces here, help us sending PRs to the GitHub [repository]!

[repository]: https://github.com/hugobessaa/elm-logoot

## Installing

Run inside your project directory:

```bash
elm package install hugobessaa/elm-logoot
```
