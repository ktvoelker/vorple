
function showCookie() {
  $("#cookie").text(document.cookie);
}

$(document).ready(function() {
  showCookie();
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
        showCookie();
        $("#result").text(JSON.stringify(d));
      },
      error: function(x, t, e) {
        showCookie();
        $("#result").text(t);
      }
    });
  });
});

