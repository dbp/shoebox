<apply template="_base">
  <div class="row">
    <h5 class="col s12">
      Found at <a href="${url}"><url/></a>
    </h5>
  </div>
  <div class="row">
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
  </div>
</apply>
