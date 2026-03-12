function moveStatusTabToRight() {
  var statusLink = document.querySelector(
    '.navbar .navbar-nav a[data-value="status"], .navbar .navbar-nav a[href="#shiny-tab-status"]',
  );
  if (!statusLink) return;

  var statusItem = statusLink.closest("li");
  if (!statusItem) return;

  var collapse = statusItem.closest(".navbar-collapse") || document.querySelector(".navbar-collapse");
  if (!collapse) return;

  var rightList = collapse.querySelector("ul.nav.navbar-nav.navbar-right.status-nav-right");
  if (!rightList) {
    rightList = document.createElement("ul");
    rightList.className = "nav navbar-nav navbar-right status-nav-right";
    collapse.appendChild(rightList);
  }

  if (statusItem.parentElement !== rightList) {
    rightList.appendChild(statusItem);
  }

  return true;
}

function renderStatusIndicator(level, text) {
  var statusLink = document.querySelector(
    '.navbar .navbar-nav a[data-value="status"], .navbar .navbar-nav a[href="#shiny-tab-status"]',
  );
  if (!statusLink) return;

  var color = "#9ca3af";
  if (level === "ok") color = "#22c55e";
  if (level === "warning") color = "#f59e0b";
  if (level === "danger") color = "#ef4444";

  var note = text ? "<small style='margin-left:6px;opacity:.85;'>" + text + "</small>" : "";
  statusLink.innerHTML =
    "<span style='display:inline-block;width:8px;height:8px;border-radius:50%;margin-right:6px;background:" +
    color +
    ";vertical-align:middle;'></span>Status" +
    note;
}

function scheduleStatusTabMove(maxAttempts, delayMs) {
  var attempts = 0;
  var timer = window.setInterval(function () {
    attempts += 1;
    if (moveStatusTabToRight() || attempts >= maxAttempts) {
      window.clearInterval(timer);
    }
  }, delayMs);
}

document.addEventListener("DOMContentLoaded", function () {
  moveStatusTabToRight();
  scheduleStatusTabMove(10, 300);
});

window.addEventListener("hashchange", function () {
  moveStatusTabToRight();
  scheduleStatusTabMove(6, 250);
});

Shiny.addCustomMessageHandler("status_nav_indicator", function (message) {
  renderStatusIndicator(message.level, message.text);
  moveStatusTabToRight();
});
