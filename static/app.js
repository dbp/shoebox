// Lazy loading of images
window.layzr = new Layzr();

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

    document.querySelector(".dropzone").classList.add("spinner");

    // NOTE(dbp 2017-05-29): Async on readEntries() and file() make what should
    // be very simple (just get all the files, then upload them one by one) much
    // more complex.

    // 1. List entries (files & directories), storing them in fileQueue / dirQueue respectively
    // 2. For dirQueue, read and add files to fileQueue, dirs to dirQueue (and recur)
    // 3. For fileQueue, get File from them and put in uploadQueue.
    // 4. For uploadQueue, upload one by one.

    var dirQueue = [];
    var fileQueue = [];
    var uploadQueue = [];

    // 1.
    var items = e.dataTransfer.items;
    for (var i = 0; i < items.length; i++) {
        var entry = items[i].webkitGetAsEntry();
        if (entry !== null) {
            if (entry.isDirectory) {
                dirQueue.push(entry);
            } else if (entry.isFile) {
                fileQueue.push(entry);
            }
        }
    }

    // 2.
    function dirs() {
        d = dirQueue.shift();
        if (typeof d !== "undefined") {
            let reader = d.createReader();
            reader.readEntries(function(entries) { // ASYNC
                entries.forEach(function(entry) {
                    if (entry.isDirectory) {
                        dirQueue.push(entry);
                    } else if (entry.isFile) {
                        fileQueue.push(entry);
                    }
                });
                dirs();
            });

        } else {
            files();
        }
    }
    dirs();

    // 3.
    function files() {
        f = fileQueue.shift();
        if (typeof f !== "undefined") {
            f.file(function(file) { // ASYNC
                uploadQueue.push(file);
                files();
            });
        } else {
            uploadQueue.forEach(function (f) {
                var e = document.createElement("li");
                e.innerHTML = f.name;
                document.querySelector(".dropzone .queue").appendChild(e);
            });
            upload();
        }
    }

    // 4.
    function upload() {
        f = uploadQueue.shift();
        if (typeof f !== "undefined") {
            var data = new FormData();
            data.append('file', f, f.name);
            var request = new XMLHttpRequest();
            request.open('POST', '/upload', true);
            request.onreadystatechange = function() { // ASYNC
	              if (request.readyState > 3 && request.status === 200) {
                    document.querySelectorAll(".dropzone .queue li").forEach(function (e) {
                        if (e.innerHTML === f.name) {
                            e.classList.add("done");
                            window.setTimeout(function () {
                                e.remove();
                            }, 500);
                        }
                    });
                    upload();
                }
	          };
            request.send(data);
        } else {
            window.location.reload(true);
        }
    }
});
window.addEventListener("dragover", function(e) {
    e.preventDefault();
    e.stopPropagation();
});

document.addEventListener('keydown', function(e) {
    if (e.key === "Escape") {
        if (window.location.pathname !== "/") {
            window.setTimeout(function () {
                window.history.go(-1);
            }, 0);
        } else {
            document.querySelector("input").blur();
            document.querySelector("input").value = "";
        }
    }
});
