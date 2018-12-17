<apply template="_base">
  <span id="boxref" data-ref="${box-ref}"/>
  <script>window.box_ref = document.getElementById("boxref").getAttribute("data-ref");</script>
  <div class="row">
      <not-editable>
        <div class="col s12">
          <h4><box-title/></h4>
        </div>
      </not-editable>
      <is-editable>
        <form class="col s6" action="/${box-ref}/title">
          <div class="row no-marg-bot">
            <div class="col s7">
              <div class="row no-marg-bot">
                <div class="col s12"
                  <label for="title">Title</label>
                  <input name="title" type="text" value="${box-title}"/>
                </div>
              </div>
              <div class="row no-marg-bot">
                <div class="input-field col s12">
                  <button class="btn btn-small waves-effect waves-light" type="submit" name="action">Save
                    <i class="material-icons right">save</i>
                  </button>
                </div>
              </div>
            </div>
            <div class="col s4">
              <preview>
                <div class="card">
                  <div class="card-image">
                    <img src="/static/icon.png" data-layzr="/${ref}/thumb"/>
                  </div>
                </div>
              </preview>
            </div>
          </div>
        </form>
        <ul class="col s6 collection">
          <urls>
            <li class="collection-item"><a href="/${url}">/<url/></a> <a href="/${url-ref}/delete" class="secondary-content" onclick="return confirm('Are you sure?');"><i class="material-icons right">delete</i></a></li>
          </urls>
          <li class="collection-item">
            <form action="/url/new" class="no-marg-bot">
              <input type="hidden" name="ref" value="${box-ref}"/>
              <div class="row no-marg-bot">
                <div class="input-field col s7">
                  <label for="url">Url</label>
                  <input type="text" name="url"/>
                </div>
                <div class="input-field col s5">
                  <button class="btn btn-small waves-effect waves-light" type="submit" name="action">New
                    <i class="material-icons right">add</i>
                  </button>
                </div>
              </div>
            </form>
          </li>
        </ul>
      </is-editable>
  </div>

  
  <div class="row">
    <items>
      <div class="col s3">
        <div class="card hoverable">
          <div class="card-image">
            <a class="thumb gallery" href="/file/${contentRef}">
              <has-thumbnail>
                <img src="/static/icon.png" data-layzr="/${contentRef}/thumb"/>
              </has-thumbnail>
              <no-thumbnail>
                <has-preview>
                  <span class="card-title">No thumbnail</span>
                </has-preview>
              </no-thumbnail>
            </a>
          </div>
          <has-preview>
            <div class="card-content">
              <p><preview/></p>
            </div>
          </has-preview>
          <is-editable>
            <div class="card-action">
              <a href="/${box-ref}/remove/${contentRef}" onclick="return confirm('Are you sure?');"><i class="material-icons right">delete</i></a>
              <a href="/${box-ref}/preview/${contentRef}"><i class="material-icons right">favorite</i></a>
          </is-editable>

            </div>
        </div>
      </div>
    </items>
  </div>
</apply>
