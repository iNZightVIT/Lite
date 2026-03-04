(function (i, s, o, g, r, a, m) {
  i["GoogleAnalyticsObject"] = r;
  (i[r] =
    i[r] ||
    function () {
      (i[r].q = i[r].q || []).push(arguments);
    }),
    (i[r].l = 1 * new Date());
  (a = s.createElement(o)), (m = s.getElementsByTagName(o)[0]);
  a.async = 1;
  a.src = g;
  m.parentNode.insertBefore(a, m);
})(
  window,
  document,
  "script",
  "https://www.google-analytics.com/analytics.js",
  "ga"
);

ga("create", "UA-82492807-2", "auto");

// Send version as an event parameter
// This doesn't require pre-configuration in Google Analytics admin panel
// The version will be available as an event parameter you can use in reports
if (typeof window.INZIGHT_LITE_VERSION !== "undefined") {
  // Send pageview with version in the path (works immediately)
  ga("send", "pageview", {
    page: window.location.pathname + "?v=" + encodeURIComponent(window.INZIGHT_LITE_VERSION)
  });
  // Also send as a custom event with version as event parameter
  ga("send", "event", {
    eventCategory: "Version",
    eventAction: "Page Load",
    eventLabel: window.INZIGHT_LITE_VERSION
  });
} else {
  ga("send", "pageview");
}
