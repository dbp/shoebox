<apply template="_base">
  <span id="boxref" data-ref="${box-ref}"/>
  <script>window.box_ref = document.getElementById("boxref").getAttribute("data-ref");</script>
  <h3><box-title/></h3>
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
        <a href="/${box-ref}/remove/${contentRef}" class="delete" onclick="return confirm('Are you sure?');">x</a>
      </li>
    </items>
  </ul>

</apply>
