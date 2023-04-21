// display a nice message when the user is disconnected
$(function () {
  $(document).on("shiny:disconnected", function (e) {
    let log = JSON.parse($("#shinylogs-tracking").html());
    $("body").load("disconnectedModal.html", function () {
      $("#sessionId").html(log.session_id);
    });
  });
});
