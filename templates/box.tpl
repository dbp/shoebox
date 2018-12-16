<apply template="_base">
  <span id="boxref" data-ref="${box-ref}"/>
  <script>window.box_ref = document.getElementById("boxref").getAttribute("data-ref");</script>
  <h3>
    <not-editable><box-title/></not-editable>
    <is-editable>
      <form action="/${box-ref}/title">
        <textarea name="title"><box-title/></textarea>
        <button type="submit">Change Title</button>
      </form>
    </is-editable>
  </h3>
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
        <is-editable>
          <a href="/${box-ref}/remove/${contentRef}" class="delete" onclick="return confirm('Are you sure?');">x</a>
        </is-editable>
      </li>
    </items>
  </ul>

</apply>
