
$(document).ready(function() {
  $("#cookie").text(document.cookie);
  $("#reset").click(function() {
    document.cookie = "";
  });
  $("#send").click(function() {
    $.ajax("/test/bin/main", {
      dataType: "json",
      contentType: "application/json",
      processData: false,
      data: $("#query").val(),
      type: 'POST',
      success: function(d) {
        $("#result").text(JSON.stringify(d));
      },
      error: function(x, t, e) {
        $("#result").text(t);
      }
    });
  });
});

