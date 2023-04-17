// display a nice message when the user is disconnected
$(function () {
  $(document).on("shiny:disconnected", function (event) {
    alert("disconnected!");
  });
});
