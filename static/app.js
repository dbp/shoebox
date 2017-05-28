/* lastTarget is set first on dragenter, then
   compared with during dragleave. */
var lastTarget = null;

window.addEventListener("dragenter", function(e) {
    lastTarget = e.target; // cache the last target here
    // unhide our dropzone overlay
    document.querySelector(".dropzone").style.visibility = "";
    document.querySelector(".dropzone").style.opacity = 1;
});

window.addEventListener("dragleave", function(e) {
    // this is the magic part. when leaving the window,
    // e.target happens to be exactly what we want: what we cached
    // at the start, the dropzone we dragged into.
    // so..if dragleave target matches our cache, we hide the dropzone.
    if(e.target === lastTarget) {
        document.querySelector(".dropzone").style.visibility = "hidden";
        document.querySelector(".dropzone").style.opacity = 0;
    }
});

window.addEventListener("drop", function(e) {
    e.preventDefault();
    e.stopPropagation();

    var data = new FormData();

    var files = e.dataTransfer.files;
    for (var i = 0; i < files.length; i++) {
        data.append('file', files[i], files[i]["name"]);
    }
    var request = new XMLHttpRequest();
    request.open('POST', '/upload', true);
    request.onreadystatechange = function() {
	      if (request.readyState > 3 && request.status === 200) {
            window.location.reload(true);
        }
	  };
    request.send(data);


    document.querySelector(".dropzone").style.visibility = "hidden";
    document.querySelector(".dropzone").style.opacity = 0;
});
window.addEventListener("dragover", function(e) {
    e.preventDefault();
    e.stopPropagation();
});
