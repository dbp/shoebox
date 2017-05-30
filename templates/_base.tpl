<!doctype html>
<html>
  <head>
    <link rel='stylesheet' href='/static/main.css'/>
  </head>
  <body>
    <div style="visibility:hidden; opacity:0" class="dropzone">
      <ul class="queue"></ul>
    </div>
    <form action="/search">
      <input name="q" type="text" placeholder="SEARCH..." value="${q}"/>
      <button class="index" onclick="return index();"></button>
      <a href="/wipe" class="wipe" onclick="return confirm('Are you sure you want to wipe the index?');"></a>
    </form>
    <apply-content/>
  </body>
  <script type="text/javascript" src="/static/layzr.min.js"></script>
  <script type="text/javascript" src="/static/app.js"></script>
</html>
