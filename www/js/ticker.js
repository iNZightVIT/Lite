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
        // if the ticker is not valid, don't show it
        if (
          new Date(value.validFrom) > new Date() ||
          new Date(value.validTo) < new Date()
        )
          return;

        if (value.target && value.target !== window.origin) return;

        // create a div with class item
        $(".ticker").append(
          "<div class='ticker-item ticker-" +
            value.type +
            "' id='tickerItem" +
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
