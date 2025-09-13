!(function () {
  "use strict";
  var r = window.location,
    i = window.document,
    e = i.currentScript,
    o = e.getAttribute("data-api") || new URL(e.src).origin + "/api/event",
    l = e.getAttribute("data-domain");
  function s(e, t) {
    e && console.warn("Ignoring Event: " + e), t && t.callback && t.callback();
  }
  function t(e, t) {
    if (
      /^localhost$|^127(\.[0-9]+){0,2}\.[0-9]+$|^\[::1?\]$/.test(r.hostname) ||
      "file:" === r.protocol
    )
      return s("localhost", t);
    if (
      (window._phantom ||
        window.__nightmare ||
        window.navigator.webdriver ||
        window.Cypress) &&
      !window.__plausible
    )
      return s(null, t);
    try {
      if ("true" === window.localStorage.plausible_ignore)
        return s("localStorage flag", t);
    } catch (e) {}
    var n = {},
      a =
        ((n.n = e),
        (n.u = r.href),
        (n.d = l),
        (n.r = i.referrer || null),
        t && t.meta && (n.m = JSON.stringify(t.meta)),
        t && t.props && (n.p = t.props),
        new XMLHttpRequest());
    a.open("POST", o, !0),
      a.setRequestHeader("Content-Type", "text/plain"),
      a.send(JSON.stringify(n)),
      (a.onreadystatechange = function () {
        4 === a.readyState &&
          t &&
          t.callback &&
          t.callback({ status: a.status });
      });
  }
  var n = (window.plausible && window.plausible.q) || [];
  window.plausible = t;
  for (var a, u = 0; u < n.length; u++) t.apply(this, n[u]);
  function p() {
    a !== r.pathname && ((a = r.pathname), t("pageview"));
  }
  function c() {
    p();
  }
  var f,
    e = window.history;
  function d(e) {
    return e && e.tagName && "a" === e.tagName.toLowerCase();
  }
  e.pushState &&
    ((f = e.pushState),
    (e.pushState = function () {
      f.apply(this, arguments), c();
    }),
    window.addEventListener("popstate", c)),
    "prerender" === i.visibilityState
      ? i.addEventListener("visibilitychange", function () {
          a || "visible" !== i.visibilityState || p();
        })
      : p();
  var v = 1;
  function w(e) {
    ("auxclick" === e.type && e.button !== v) ||
      ((e = (function (e) {
        for (; e && (void 0 === e.tagName || !d(e) || !e.href); )
          e = e.parentNode;
        return e;
      })(e.target)) &&
        e.href &&
        e.href.split("?")[0],
      (function e(t, n) {
        if (!t || h < n) return !1;
        if (y(t)) return !0;
        return e(t.parentNode, n + 1);
      })(e, 0));
  }
  function m(e, t, n) {
    var a,
      r = !1;
    function i() {
      r || ((r = !0), (window.location = t.href));
    }
    !(function (e, t) {
      if (!e.defaultPrevented)
        return (
          (t = !t.target || t.target.match(/^_(self|parent|top)$/i)),
          (e = !(e.ctrlKey || e.metaKey || e.shiftKey) && "click" === e.type),
          t && e
        );
    })(e, t)
      ? ((a = { props: n.props }), plausible(n.name, a))
      : ((a = { props: n.props, callback: i }),
        plausible(n.name, a),
        setTimeout(i, 5e3),
        e.preventDefault());
  }
  function g(e) {
    var e = y(e) ? e : e && e.parentNode,
      t = { name: null, props: {} },
      n = e && e.classList;
    if (n)
      for (var a = 0; a < n.length; a++) {
        var r,
          i = n.item(a).match(/plausible-event-(.+)(=|--)(.+)/);
        i &&
          ((r = i[1]),
          (i = i[3].replace(/\+/g, " ")),
          "name" == r.toLowerCase() ? (t.name = i) : (t.props[r] = i));
      }
    return t;
  }
  i.addEventListener("click", w), i.addEventListener("auxclick", w);
  var h = 3;
  function b(e) {
    if ("auxclick" !== e.type || e.button === v) {
      for (var t, n, a, r, i = e.target, o = 0; o <= h && i; o++) {
        if ((a = i) && a.tagName && "form" === a.tagName.toLowerCase()) return;
        d(i) && (t = i), y(i) && (n = i), (i = i.parentNode);
      }
      n &&
        ((r = g(n)),
        t
          ? ((r.props.url = t.href), m(e, t, r))
          : (((e = {}).props = r.props), plausible(r.name, e)));
    }
  }
  function y(e) {
    var t = e && e.classList;
    if (t)
      for (var n = 0; n < t.length; n++)
        if (t.item(n).match(/plausible-event-name(=|--)(.+)/)) return !0;
    return !1;
  }
  i.addEventListener("submit", function (e) {
    var t,
      n = e.target,
      a = g(n);
    function r() {
      t || ((t = !0), n.submit());
    }
    a.name &&
      (e.preventDefault(),
      (t = !1),
      setTimeout(r, 5e3),
      (e = { props: a.props, callback: r }),
      plausible(a.name, e));
  }),
    i.addEventListener("click", b),
    i.addEventListener("auxclick", b);
})();

window.plausible =
  window.plausible ||
  function () {
    (window.plausible.q = window.plausible.q || []).push(arguments);
  };

$(function () {
  // do some event tracking
  $(document).on("shiny:connected", function (e) {
    if (window.plausible) plausible("Connect");
  });

  $(document).on("shiny:inputchanged", function (e) {
    if (window.plausible) {
      if (e.name === "vari1") plausible("Variable 1");
      if (e.name === "vari2") plausible("Variable 2");

      if (e.name === "plot_selector") plausible("Plot selector: " + e.value);
    }
  });
});
