# Shoebox

Shoebox is a from-scratch re-implementation of Camlistore in Haskell. Camlistore is
a really cool project, but the actual implementation has always been somewhat
unreliable to me (not so much the core, but the indexer layer & frontend, which
is critical to actually use the thing). It's also a massive codebase 
(`git clone git@github.com:camlistore/camlistore.git && cd camlistore && cloc app bin clients cmd dev lib misc pkg server`, 
which should be a decent approximation and excludes vendored libraries, reports almost 93K lines of Go), written in
a language I don't like (Go).

Further, Shoebox has somewhat different goals:

1. Beyond the blob store, there is no particular effort to make this backend
   agnostic. Camlistore has the indexer based on anything that is a K/V store,
   but as a result, it means that all higher logic is in turn encoded on top of
   that (and, these layers are entirely undocumented -- at least, with Shoebox, you
   can inspect the schema of the indexer and pretty much understand what's going
   on! Apples to Oranges, as Shoebox is doing much less currently, but the idea
   stands). This complicates things in a way that doesn't seem warranted (the
   complication of encoding everything as content addressable blobs, on the
   other hand, is clearly warranted!)
2. Shoebox does not care about sharing. It's primary goal is to be able to dump
   data into it and then process (& archive) it. Being online at all is, at
   least for now, a non-goal, except insofar as a browser is used as an
   interface. Sharing seems to be where the majority of recent effort on the main
   Camlistore implementation has gone. Whereas, by contrast, ever though there
   was discussion of it years ago, Camlistore still doesn't have any support for
   storing email (which Shoebox does!). Related to not being about sharing, Shoebox
   does care about being a useful interface to odd formats. By having a
   completely ephemeral blob storage and index, you can start a Shoebox instance,
   dump in anything that it can understand (which right now is limited to
   images, mbox files full of email, and zip files with anything else Shoebox
   understands inside) and then use the interface to explore the data. 
3. Shoebox is not focused on non-technical users. Recently, a lot of effort in the
   main Camlistore implementation has been to be able to spin up servers on
   Google cloud. This is, from Shoebox's perspective, a non-feature. Running on our
   own computers is our focus. Eventually, we will want to build support for
   different blob stores (including online ones), and replication, but first, it
   has to work incredibly well as a local place to dump files and organize them.
   On the other hand, for technical users Shoebox aims to be much easier to work
   with. In particular, we want it to be trivial to spin up a new ephemeral
   server to experiment with, we want the indexer to be reliable, and the code
   should be as small as is reasonable. 
   
## Status

Right now, Shoebox is extremely new. It's currently quite a hack. It has no
automated tests. It was written in three days. But I can point it at a blob
store of files that I put into Camlistore a few years ago and look at pictures
(which current Camlistore no longer seems to be able to do -- the indexer says
it processes everything, but no search queries produce anything, including
"is:image"). And it does some things that Camlistore doesn't -- I can drop .mbox
files on it and have emails show up, I can drag entire folders (and zip files)
and have it churn through them. 


## Code Demographics

(via `cloc src app migrations static`, last run `6/5/2017`)

```
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
Haskell                         17            176             20           1113
CSS                              1             22              7            168
JavaScript                       2             15             24            121
SQL                              5              2              1             28
C                                1              7              0             24
-------------------------------------------------------------------------------
SUM:                            26            222             52           1454
-------------------------------------------------------------------------------
```

## Time Spent (hrs)

```
5/26: 2
5/27: 10
5/28: 7
5/29: 9
5/30: 1
5/31: 2
6/5: 1
```
