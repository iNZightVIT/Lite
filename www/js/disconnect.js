// display a nice message when the user is disconnected
$(function () {
  $(document).on("shiny:disconnected", function (e) {
    $("body").load("disconnectedModal.html", function () {
      // TODO: get the session id from the server
      $("#sessionId").html("DFHK3s1");
    });
  });
});
