
$(document).ready(function() {
  $("#send").click(function() {
    $.ajax("/test/bin/main/munge", {
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

