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
      </li>
    </items>
    <li class="file">
      <form action="/new" class="new">
        <label for="title">Title
          <textarea name="title"></textarea>
        </label>
        <br/><br/>
        <button type="submit">add new box</button>
      </form>
    </li>
  </ul>

  <has-more>
    <a href='?page=${next-page}'>More</a>
  </has-more>
</apply>
