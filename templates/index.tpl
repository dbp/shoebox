<!doctype html>
<html>
  <head>
    <link rel='stylesheet' href='/static/main.css'/>
  </head>
  <body>
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
</html>
