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
  var HEARTBEAT_INTERVAL_MS = 20000; // 20 seconds
  var heartbeatTimer = null;
  var lastHeartbeatTime = 0;
  var sessionStartTime = Date.now();

  // Set up dataLayer queue before gtag.js loads
  window.dataLayer = window.dataLayer || [];
  function gtag() {
    window.dataLayer.push(arguments);
  }
  window.gtag = gtag;

  // Track page load performance
  if (window.performance && window.performance.timing) {
    window.addEventListener("load", function () {
      setTimeout(function () {
        var perf = window.performance.timing;
        var pageLoadTime = perf.loadEventEnd - perf.navigationStart;
        var domContentLoaded = perf.domContentLoadedEventEnd - perf.navigationStart;

        // Store for later use in events
        window._gaPageLoadTime = pageLoadTime;
        window._gaDOMContentLoaded = domContentLoaded;
      }, 0);
    });
  }

  // Track JavaScript errors
  window.addEventListener("error", function (e) {
    if (window.gtag && typeof window.gtag === "function") {
      gtag("event", "exception", {
        description: e.message + " at " + (e.filename || "") + ":" + (e.lineno || ""),
        fatal: false,
      });
    }
  });

  // Load gtag.js
  var script = document.createElement("script");
  script.async = true;
  script.src = "https://www.googletagmanager.com/gtag/js?id=" + GA4_ID;
  script.onerror = function () {
    // gtag.js blocked (ad blocker, network error, etc.) — fail silently
  };
  script.onload = function () {
    gtag("js", new Date());

    // Set user properties for the session (will be included in all events including heartbeats)
    if (typeof window.INZIGHT_LITE_VERSION !== "undefined") {
      gtag("set", { app_version: window.INZIGHT_LITE_VERSION });
    }

    var configParams = { send_page_view: false };
    gtag("config", GA4_ID, configParams);

    // Send initial page view with additional metadata
    var pageViewParams = {
      page_path: window.location.pathname,
      session_start_time: sessionStartTime,
    };

    // Include performance metrics if available
    if (window._gaPageLoadTime) {
      pageViewParams.page_load_time_ms = window._gaPageLoadTime;
      pageViewParams.dom_content_loaded_ms = window._gaDOMContentLoaded;
    }

    gtag("event", "page_view", pageViewParams);

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

  // Track page unload to capture final session metrics
  window.addEventListener("beforeunload", function () {
    if (window.gtag && typeof window.gtag === "function") {
      var sessionDuration = Date.now() - sessionStartTime;
      gtag("event", "session_end", {
        session_duration_ms: sessionDuration,
        transport_type: "beacon",
      });
    }
  });

  // Also track when page is being unloaded (more reliable than beforeunload)
  window.addEventListener("pagehide", function () {
    if (window.gtag && typeof window.gtag === "function") {
      var sessionDuration = Date.now() - sessionStartTime;
      gtag("event", "session_end", {
        session_duration_ms: sessionDuration,
        transport_type: "beacon",
      });
    }
  });
})();
