// get logId from GET
const params = new URLSearchParams(window.location.search);
if (params.has("logId")) {
  const id = params.get("logId");
  window.location = "/logs/lite_logs_" + id + ".json";
}
