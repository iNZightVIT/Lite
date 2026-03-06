// === Universal Analytics (legacy) ===
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

if (typeof window.INZIGHT_LITE_VERSION !== "undefined") {
  ga("send", "pageview", {
    page:
      window.location.pathname +
      "?v=" +
      encodeURIComponent(window.INZIGHT_LITE_VERSION),
  });
  ga("send", "event", {
    eventCategory: "Version",
    eventAction: "Page Load",
    eventLabel: window.INZIGHT_LITE_VERSION,
  });
} else {
  ga("send", "pageview");
}

// === GA4 with heartbeat for real-time user tracking ===
(function () {
  var GA4_ID = "G-0HN36ZDTGV";
  var HEARTBEAT_INTERVAL_MS = 60000;
  var heartbeatTimer = null;
  var lastHeartbeatTime = 0;

  // Set up dataLayer queue before gtag.js loads
  window.dataLayer = window.dataLayer || [];
  function gtag() {
    window.dataLayer.push(arguments);
  }
  window.gtag = gtag;

  // Load gtag.js
  var script = document.createElement("script");
  script.async = true;
  script.src = "https://www.googletagmanager.com/gtag/js?id=" + GA4_ID;
  script.onerror = function () {
    // gtag.js blocked (ad blocker, network error, etc.) — fail silently
  };
  script.onload = function () {
    gtag("js", new Date());
    gtag("config", GA4_ID, { send_page_view: false });

    // Send initial page view with version
    var pageParams = { page_path: window.location.pathname };
    if (typeof window.INZIGHT_LITE_VERSION !== "undefined") {
      pageParams.app_version = window.INZIGHT_LITE_VERSION;
    }
    gtag("event", "page_view", pageParams);

    lastHeartbeatTime = Date.now();
    startHeartbeat();
  };
  document.head.appendChild(script);

  function sendHeartbeat() {
    gtag("event", "hb", {
      engagement_time_msec: HEARTBEAT_INTERVAL_MS,
    });
    lastHeartbeatTime = Date.now();
  }

  function startHeartbeat() {
    if (heartbeatTimer) return;
    heartbeatTimer = setInterval(sendHeartbeat, HEARTBEAT_INTERVAL_MS);
  }

  function stopHeartbeat() {
    if (heartbeatTimer) {
      clearInterval(heartbeatTimer);
      heartbeatTimer = null;
    }
  }

  // Pause heartbeat when tab is hidden, resume when visible.
  // On hide, send a final beacon with actual elapsed engagement time.
  document.addEventListener("visibilitychange", function () {
    if (document.hidden) {
      stopHeartbeat();
      var elapsed = lastHeartbeatTime > 0 ? Date.now() - lastHeartbeatTime : 1;
      gtag("event", "hb", {
        engagement_time_msec: elapsed,
        transport_type: "beacon",
      });
    } else {
      lastHeartbeatTime = Date.now();
      startHeartbeat();
    }
  });
})();
