// display a nice message when the user is disconnected
$(function () {
  const url =
    "https://raw.githubusercontent.com/iNZightVIT/inzight-www/main/iNZight/lite/ticker.json";
  $(document).on("shiny:disconnected", function (e) {
    // let log = JSON.parse($("#shinylogs-tracking").html());
    $("body").load("disconnectedModal.html", function () {
      $.getJSON(url, function (tickerData) {
        if (tickerData.length === 0) return;

        // loop through the ticker data
        $.each(tickerData, function (index, value) {
          // if the ticker is not valid, don't show it
          if (
            new Date(value.validFrom) > new Date() ||
            new Date(value.validTo) < new Date()
          )
            return;

          // create a div with class item
          $("#ticker").append(
            "<div class='ticker-panel'>\
                <div class='ticker-heading'>" +
              '<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="ticker-icon">\
  <path stroke-linecap="round" stroke-linejoin="round" d="M11.25 11.25l.041-.02a.75.75 0 011.063.852l-.708 2.836a.75.75 0 001.063.853l.041-.021M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-9-3.75h.008v.008H12V8.25z" />\
</svg>' +
              value.title +
              "</div>" +
              "<div class='ticker-body'>" +
              value.message +
              "</div></div>"
          );
          $("#tickerItem" + index + " .close").on("click", function () {
            $("#tickerItem" + index).slideUp();
          });
          $("#tickerItem" + index).slideDown();
        });
      });
    });

    // $("#sessionId").html(log.session_id);
    // $("#dlLogButton").attr("href", log.download_path);
  });
});
