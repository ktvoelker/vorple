
$(document).ready(function() {
  $("#init").click(function() {
    $.ajax("/test/bin/main/init", {
      type: 'POST',
      success: function(d) {
        $("#result").text(JSON.stringify(d));
      },
      error: function(x, t, e) {
        $("#result").text(t);
      }
    });
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

