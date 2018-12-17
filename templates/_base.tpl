<!doctype html>
<html>
  <head>
    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    <link rel='stylesheet' href='/static/materialize.min.css'>
    <link rel='stylesheet' href='/static/main.css'/>
    <link rel='stylesheet' href='/static/luminous-basic.min.css'>
    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  </head>
  <body>
    <div style="visibility:hidden; opacity:0" class="dropzone">
      <ul class="queue"></ul>
    </div>
    <nav class="light-blue lighten-1" role="navigation">
      <div class="nav-wrapper container"><a id="logo-container" href="/" class="brand-logo">Shoebox</a>
        <is-editable>
          <ul class="right">
            <li><a href="/reindex" class="btn-floating btn-small waves-effect waves-light red"><i class="material-icons right">update</i></a></li>
            <li><a href="/wipe" class="btn-floating btn-small waves-effect waves-light red" onclick="return confirm('Are you sure you want to wipe the index?');"><i class="material-icons right">delete_forever</i></a></li>
          </ul>
        </is-editable>
      </div>
    </nav>

    <div class="section">
      <div class="container">
        <apply-content/>
      </div>
    </div>
  </body>
  <footer class="page-footer">
    <div class="container">
      <div class="row">
        <div class="col s12">
          <p class="grey-text text-lighten-4">Shoebox is a durable repository for human-scale collections of files, like the shoebox full of old letters and family photos discovered 50 years later. It has a simple core data format that is easy to store and migrate between different places (currently supporting file system directories, Amazon S3, and transient in-memory) and mostly immutable so that synchronizing is trivial. The data format is also intended to support data archeology, in that it is primarily plain-text and relatively straightforward. Layered on top is a performant indexing system (currently, PostgreSQL and SQLite are supported), and a web application as a frontend. Shoebox is very heavily inspired by <a class="grey-text text-lighten-3" href=https://perkeep.org/">Perkeep</a>, and indeed began as a re-implementation, but now differs in several important ways.</p>
        </div>
      </div>
    </div>
    <div class="footer-copyright">
      <div class="container">
        © 2018 Daniel Patterson <a target="_blank" class="right grey-text text-lighten-3" href="https://github.com/dbp/shoebox">github.com/dbp/shoebox</a>
      </div>
    </div>
  </footer>

  <script type="text/javascript" src="/static/materialize.min.js"></script>
  <script type="text/javascript" src="/static/layzr.min.js"></script>
  <script type="text/javascript" src="/static/Luminous.min.js"></script>
  <script type="text/javascript" src="/static/app.js"></script>
</html>
