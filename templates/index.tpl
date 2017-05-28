<!doctype html>
<html>
  <head>
    <link rel='stylesheet' href='/static/main.css'/>
  </head>
  <body>
    <div style="visibility:hidden; opacity:0" class="dropzone"></div>
    <ul>
      <files>
        <li>
          <is-file>
            <a href='/${sha}'>
              <img src='/${sha}/thumb'/>
              <name/>
            </a>
          </is-file>
          <not-file>
            Not a file.
          </not-file>
        </li>
      </files>
    </ul>
    <a href='?page=${next-page}'>More</a>
  </body>
  <script type="text/javascript" src="/static/app.js"></script>
</html>
