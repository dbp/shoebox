<apply template="_base">
  <div class="row">
    <not-editable>
      <div class="col s12">
        <h5>Nothing to see</h5>
        <p class="grey-text">
        This is a private shoebox. If you have a link to content, please use it
        directly, as there is no other way to navigate to that content.
      </p>
    </not-editable>
    <is-editable>
      <items>
        <div class="col s3">
          <div class="card">
            <div class="card-image">
              <a class="thumb" href="/${contentRef}">
                <has-thumbnail>
                  <img src="/static/icon.png" data-layzr="/${contentRef}/thumb"/>
                  <has-preview>
                    <span class="card-title"><preview/></span>
                  </has-preview>
                </has-thumbnail>
                <no-thumbnail>
                  <has-preview>
                    <span class="card-title">No thumbnail -- <preview/></span>
                  </has-preview>
                </no-thumbnail>
              </a>
            </div>
          </div>
        </div>
      </items>
      <div class="col s3">
        <div class="card hoverable">
          <div class="card-content">
            <div class="row no-marg-bot">
              <form method="post" action="/new" class="col s12 no-marg-bot">
                <div class="row no-marg-bot">
                  <div class="input-field col s12">
                    <input placeholder="Title" id="title" name="title" type="text" class="validate">
                    <label for="title">Title</label>
                  </div>
                </div>
                <div class="row">
                  <div class="input-field col s12">
                    <button class="btn waves-effect waves-light" type="submit" name="action">Add
                      <i class="material-icons right">add</i>
                    </button>
                  </div>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    </is-editable>
  </div>
</apply>
