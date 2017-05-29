<apply template="_base">
  <ul>
    <permanodes>
      <li class="file">
        <a class="info" href="/raw/${contentRef}"></a>
        <a class="thumb" href="/${contentRef}">
          <has-thumbnail>
            <img src="/static/icon.png" data-layzr="/${permanodeRef}/thumb"/>
          </has-thumbnail>
          <has-preview>
            <div class="preview">
              <p><preview/></p>
            </div>
          </has-preview>
        </a>
      </li>
    </permanodes>
  </ul>
  <has-more>
    <a href='?page=${next-page}'>More</a>
  </has-more>
</apply>
