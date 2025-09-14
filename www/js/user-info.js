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
