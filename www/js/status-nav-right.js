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
