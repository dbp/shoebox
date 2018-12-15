<apply template="_base">
  <ul>
    <items>
      <li class="file">
        <a class="thumb" href="/${contentRef}">
          <has-thumbnail>
            <img src="/static/icon.png" data-layzr="/${contentRef}/thumb"/>
          </has-thumbnail>
          <has-preview>
            <div class="preview">
              <p><preview/></p>
            </div>
          </has-preview>
        </a>
        <a href="/${contentRef}/delete" onclick="return confirm('Are you sure?');" class="delete">x</a>
      </li>
    </items>
  </ul>
  <has-more>
    <a href='?page=${next-page}'>More</a>
  </has-more>
</apply>
