function clearCookieEverywhere(cookieName) {
  var expires = "expires=Thu, 01 Jan 1970 00:00:00 GMT";
  var path = "path=/";
  var host = window.location.hostname || "";
  var domains = [];

  if (host) {
    domains.push(host);
    var parts = host.split(".");
    for (var i = 1; i < parts.length - 1; i++) {
      domains.push("." + parts.slice(i).join("."));
    }
  }

  document.cookie = cookieName + "=;" + expires + ";" + path;
  for (var j = 0; j < domains.length; j++) {
    document.cookie = cookieName + "=;" + expires + ";" + path + ";domain=" + domains[j];
  }
}

function clearAllCookies() {
  if (!document.cookie) return;
  var cookies = document.cookie.split(";");
  for (var i = 0; i < cookies.length; i++) {
    var cookie = cookies[i].trim();
    if (!cookie) continue;
    var eqPos = cookie.indexOf("=");
    var name = eqPos > -1 ? cookie.substring(0, eqPos) : cookie;
    clearCookieEverywhere(name);
  }
}

function performReconnect() {
  if (window.__inzightReconnectInProgress) return;
  window.__inzightReconnectInProgress = true;

  try {
    clearAllCookies();
  } catch (e) {
    console.warn("Unable to clear all cookies", e);
  }

  try {
    window.localStorage.clear();
    window.sessionStorage.clear();
  } catch (e) {
    console.warn("Unable to clear browser storage", e);
  }

  var reconnectUrl;
  try {
    var url = new URL(window.location.href);
    url.searchParams.set("_reconnect", Date.now().toString());
    url.hash = "";
    reconnectUrl = url.toString();
  } catch (e) {
    var reconnectPath = window.location.pathname || "/";
    var currentSearch = window.location.search || "";
    var params = new URLSearchParams(currentSearch);
    params.set("_reconnect", Date.now().toString());
    reconnectUrl = reconnectPath + "?" + params.toString();
  }

  // Force immediate top-level navigation.
  window.location.replace(reconnectUrl);

  // Fallback in case replace is interrupted.
  window.setTimeout(function () {
    try {
      window.location.href = reconnectUrl;
    } catch (e) {}
  }, 80);
}

Shiny.addCustomMessageHandler("force_reconnect", function () {
  performReconnect();
});

function shouldTriggerReconnectFromHash() {
  return window.location.hash === "#shiny-tab-reconnect";
}

function maybeReconnectFromHash() {
  if (shouldTriggerReconnectFromHash()) {
    performReconnect();
  }
}

// Trigger reconnect when tab hash changes to reconnect.
window.addEventListener("hashchange", maybeReconnectFromHash);
window.setTimeout(maybeReconnectFromHash, 0);

// Also trigger reconnect directly from menu click to avoid
// depending on Shiny tab-selection state only.
document.addEventListener("click", function (event) {
  var target = event.target;
  if (!(target instanceof Element)) return;
  var link = target.closest("a");
  if (!link) return;

  var href = (link.getAttribute("href") || "").trim();
  var dataValue = (link.getAttribute("data-value") || "").trim();
  var label = (link.textContent || "").trim().toLowerCase();
  var isReconnectLink =
    href === "#shiny-tab-reconnect" ||
    dataValue === "reconnect" ||
    label === "reconnect";

  if (!isReconnectLink) return;
  event.preventDefault();
  event.stopPropagation();
  performReconnect();
}, true);
