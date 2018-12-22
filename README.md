# Shoebox

Shoebox is a durable repository for human-scale collections of files, like the
shoebox full of old letters and family photos discovered 50 years later. It has
a simple core data format that is easy to store and migrate between different
places (currently both file system directories and Amazon S3, as well as an
in-memory store for testing) and mostly immutable so that synchronizing is
trivial.

The data format is designed to support data archaeology, in that it is primarily
 plain-text and should be understandable without documentation. Layered on top
 is a performant indexing system (currently PostgreSQL and SQLite are
 supported), and a web application as a frontend.

Shoebox is very heavily inspired by [Perkeep](https://perkeep.org/), and indeed
began as a re-implementation (called [Shed](https://github.com/dbp/shed), with
different goals but a compatibly data format), but now differs in emphasis and
underlying data format.

There are a few fundamental differences:

1. In an effort to have the underlying data format be more amenable to data
   archaeology (i.e., given only the data, someone could reconstruct what was
   there), Shoebox is _not_ immutable, as rather than having collections of
   items be a series of "claims" (which are a more complex thing to deal with),
   we create new collection blobs (called boxes) and mark them as _replacements_
   of old ones with replace blobs. An implementation does not need to actually
   delete the old data, but semantically it is now garbage and subject to
   garbage collection. Since we already support replacement, we also added
   support for actual delection -- this is not a common occurrence, but if you
   managed to add something to the store that you don't want, you can create a
   delete blob that indicates that another blob should be deleted. With some
   caveats, this is easy to synchronize / merge. 
   
2. Shoebox is intended for single people to upload their own data to, and then
   to provide public limited access if desired. There is a lot of complexity in
   Perkeep to support sharing, hosting others content, etc, and all of this has
   been discarded. Additionally, we have no interest in pulling in social media
   content (I don't _want_ to have apps tracking where I go to restaurants at
   all, so having it end up in Shoebox is a non-goal!). Shoebox is trying to
   recover what we used to have: photos that got developed and stuck into
   shoeboxes, and then were able to reliably be kept for decades, and that's
   pretty much it. 
   
3. This is a more minor goal, but I wanted to make it much easier to spin up
   instances and test things. As a result, we have in-memory blob stores and
   indexers by default, and if you just run the application you can dump data in
   and experiment with it. We have some experimental support for email mbox
   files (which is not fully supported anymore), and we have support for getting
   stuff that is inside zip files, so there is some possible other uses for
   shoebox as a way of exploring data, but this is all sort of in the future. 
   
## Dependencies

On a Debian-like system, these are what the packages needed are called.

```
imagemagick-dev
libmagic-dev
libexif-dev
```
