<!doctype html>
<html>
  <head>
    <link rel='stylesheet' href='/static/main.css'/>
  </head>
  <body>
    <div style="visibility:hidden; opacity:0" class="dropzone"></div>
    <ul>
      <files>
        <li class="file">
          <a class="info" href="/raw/${fileRef}"></a>
          <a class="thumb" href="/${fileRef}">
            <img src="/${permanodeRef}/thumb"/>
            <name/>
          </a>
        </li>
      </files>
    </ul>
    <a href='?page=${next-page}'>More</a>
  </body>
  <script type="text/javascript" src="/static/app.js"></script>
</html>
