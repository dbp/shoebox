- motivation

  something inspired by but simpler than perkeep/camlistore, which claims to be
  a 50year project but can't seem to manage to be stable even over a couple
  years (i.e., they just changed their hash from sha1 to sha224... so my client
  no longer works; and the different packed/nonpacked formats on disk are
  non-trivial to parse; in theory blobs are simple, but in _practice_ they are
  more complicated).

- deviations from perkeep

  - larger blob size limit: 16mb. this should make it much easier to understand
    on most systems that I know of (normal file systems, S3, etc), as it'll mean
    that most files will end up as single blobs (metadata separate, but the
    actual bytes in a single blob). This will also increase performance w/o
    needing the complexity of the packed format. The downside is that there is
    less opportunity for fancy rolling checksums, but:
    A. I don't think that's super important to me.
    B. There is no reason that for particular uses you couldn't use smaller blobs.

  - better specified up-front metadata (not primarily in code!). this is an
    archival project, so we'll start with two of the most important things that
    I'd like to never lose: photos & email.

  - a better story for _organization_ of data. in particular, while _data_
    should never be lost, organization relies upon mutation, because you want to
    rearrange things. perkeep could do this, but it felt relatively clumsy /
    bolted on after the fact. collections are a fundamentally useful way of
    organizing things, but we want the blob representation to be simple, so
    we'll do it by way of _mutation_.

  - mutation! or, we add specific support for _deleting_ blobs. This is a blob
    that asserts that another blob can be removed. Presence of these blobs can
    cause the blob server to immediately remove the blob in question. The
    deletion assertion cannot be removed in the same way, because it will need
    to replicate, so there will be a history of what was deleted that persists
    in the system (we could do garbage collection on these, but it's not clear
    if it matters -- the actual content will be gone, as the deletion assertions
    will not show up visibly in any way). Because we have this mechanism, rather
    than building up metadata blobs by sequences of deltas that are only
    reconstructed into useful data by the indexer (making the indexer much more
    complicated), a common pattern can be to create a new blob with the changed
    data and then delete the old one. 

  - need some specified notion of what the indexer will actually operate over.
    In the perkeep model, permanodes are viewed as mutable nodes, and permanodes
    are essentially what the UI shows and what the indexers computes. In our
    world, it's much simpler, but we still need a notion of what the indexer is
    caching / displaying. If we look at what our indexer actually did, for each
    "permanode", we stored:
    
    - attributes TEXT NOT NULL DEFAULT '{}'
    - show_in_ui INTEGER NOT NULL DEFAULT false
    - thumbnail BLOB
    - preview TEXT
    - search_low TEXT
    - search_high TEXT
    - content TEXT
    
    Where attributes were the arbitrary permanode attributes (i.e., from the
    perkeep spec), but the rest were essentially content agnostic features that
    made sense for presenting a UI. show_in_ui is a little confusing, as it
    seems to exist only to deal with essentially malformed blobs (permanodes
    pointing to blobs that don't exist), but the rest make sense, with content
    being the sha for the actual blob in question. 
    
    Probably we should call this something other than permanode; make it be an
    indexer-only data structure; and then have those entries be what drive the
    UI. Then the point of indexing is extracting out what is relevant to display
    in the UI. How about something generic like "item".


- blob types

  - bytes (up to 16MB)
  - image
    - filename
    - timestamp taken
    - thumbnail
    - data blobref
  - email
    - to
    - from
    - date
    - subject
    - body text
    - raw blobref
  - collection
    - title
    - list of blobref

- item spec
  - content_ref TEXT -- primary key
  - thumbnail BLOB
  - preview TEXT
  - search_low TEXT
  - search_high TEXT


- initial version

  just images (but support JPG & RAW)

