$(function () {
  const url =
    "https://raw.githubusercontent.com/iNZightVIT/inzight-www/main/iNZight/lite/ticker.json";
  $(document).ready(function () {
    // download the ticker data from https://inzight.nz/lite/ticker.json
    $.getJSON(url, function (data) {
      // create a div with class ticker in body
      $("body").prepend("<div class='ticker'></div>");

      // loop through the ticker data
      $.each(data, function (index, value) {
        // create a div with class item
        $(".ticker").append(
          "<div class='ticker-item' id='tickerItem" +
            index +
            "'>\
            <strong>" +
            value.title +
            "</strong>: " +
            value.message +
            "<div class='close'>&times;</div>" +
            "</div>"
        );
        $("#tickerItem" + index + " .close").on("click", function () {
          $("#tickerItem" + index).slideUp();
        });
        $("#tickerItem" + index).slideDown();
      });
    });
  });
});
